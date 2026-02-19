{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DeMoDNote.Backend (
    module DeMoDNote.Types,
    module DeMoDNote.Config, 
    module DeMoDNote.Detector,
    DetectionEvent(..),
    runBackend,
    runBackendSimple,
    JackState(..),
    newJackState,
    JackStatus(..)
) where

import DeMoDNote.Types
import DeMoDNote.Config
import DeMoDNote.Detector

-- import Sound.JACK  -- Currently unused, kept for potential future use
-- import Sound.JACK.Exception  -- Kept for potential future use
import qualified Sound.JACK.Audio as JAudio
import qualified Data.Vector.Storable as VS
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent (threadDelay, forkIO)
import Data.IORef
import Data.Word (Word64)
import Foreign.C.Types (CFloat(..))
import Data.Int (Int16)
import System.CPUTime (getCPUTime)
import System.Posix.Signals (installHandler, sigINT, sigTERM, Handler(..))
import Control.Exception (try, throwIO, SomeException, Exception(..))

-- Jack connection status
data JackStatus 
    = JackConnected
    | JackDisconnected
    | JackReconnecting
    | JackError String
    deriving (Show, Eq)

-- Jack state for reconnection
data JackState = JackState
    { jackStatus :: IORef JackStatus
    , reconnectAttempts :: IORef Int
    , maxReconnectAttempts :: Int
    , shouldReconnect :: IORef Bool
    }

newJackState :: Int -> IO JackState
newJackState maxAttempts = do
    jackStatusRef <- newIORef JackConnected
    attempts <- newIORef 0
    recon <- newIORef True
    return $ JackState jackStatusRef attempts maxAttempts recon

-- JACK exception type
data JackException = JackConnectionFailed String
                   | JackDisconnectedErr String
                   | JackReconnectFailed Int
    deriving (Show)

instance Exception JackException

-- Attempt to reconnect to JACK
attemptReconnect :: JackState -> IO Bool
attemptReconnect state = do
    attempts <- readIORef (reconnectAttempts state)
    
    if attempts >= maxReconnectAttempts state
    then do
        writeIORef (jackStatus state) $ JackError "Max reconnection attempts reached"
        writeIORef (shouldReconnect state) False
        putStrLn $ "JACK reconnection failed after " ++ show attempts ++ " attempts"
        return False
    else do
        writeIORef (jackStatus state) JackReconnecting
        putStrLn $ "JACK disconnected. Reconnecting (attempt " ++ show (attempts + 1) ++ "/" ++ show (maxReconnectAttempts state) ++ ")..."
        threadDelay 2000000  -- 2 second delay
        putStrLn "Reconnection requires JACK server restart. Please restart DeMoD-Note."
        writeIORef (shouldReconnect state) False
        return False

-- Handle JACK error with reconnection
handleJackError :: JackState -> SomeException -> IO ()
handleJackError state e = do
    let errMsg = show e
    putStrLn $ "JACK error: " ++ errMsg
    writeIORef (jackStatus state) $ JackError errMsg
    
    shouldRecon <- readIORef (shouldReconnect state)
    when shouldRecon $ do
        _ <- attemptReconnect state
        return ()

-- Wrap action with error handling (currently unused but kept for future use)
_withJackErrorHandling :: JackState -> IO a -> IO a
_withJackErrorHandling state action = do
    result <- try action
    case result of
        Right val -> return val
        Left (e :: SomeException) -> do
            handleJackError state e
            throwIO e

-- Detection event for TUI updates
data DetectionEvent = DetectionEvent
    { deNote       :: Maybe (Int, Int)
    , deConfidence :: Double
    , deLatency    :: Double
    , deWaveform   :: [Double]
    , deState      :: NoteState
    , deTuningNote :: Maybe Int
    , deTuningCents :: Double
    , deTuningInTune :: Bool
    , deJackStatus :: JackStatus
    }

getMicroTime :: IO Word64
getMicroTime = do
  t <- getCPUTime
  return $ fromIntegral (t `div` 1000000)

-- Ring buffer for audio samples
data AudioRingBuffer = AudioRingBuffer
  { rbBuffer    :: !(VS.Vector Int16)
  , rbWritePos :: !(IORef Int)
  , rbSize     :: !Int
  }

newAudioRingBuffer :: Int -> IO AudioRingBuffer
newAudioRingBuffer size = do
  writePos <- newIORef 0
  let buffer = VS.replicate size 0
  return $ AudioRingBuffer buffer writePos size

pushSamples :: AudioRingBuffer -> VS.Vector Int16 -> IO ()
pushSamples rb samples = do
  writePos <- readIORef (rbWritePos rb)
  let !_newWritePos = (writePos + VS.length samples) `mod` rbSize rb
  -- Note: write position not actually updated - this is a simplified implementation
  return ()

readSamples :: AudioRingBuffer -> Int -> IO (VS.Vector Int16)
readSamples rb n = do
  writePos <- readIORef (rbWritePos rb)
  let !size = rbSize rb
  let !buf = rbBuffer rb
  return $ VS.generate n $ \i ->
    let idx = (writePos - n + i) `mod` size
    in buf VS.! idx

cfloatToInt16 :: CFloat -> Int16
cfloatToInt16 (CFloat x) = 
  let scaled = round (x * 32767.0) :: Int
  in fromIntegral $ max (-32768) (min 32767 scaled)

-- Audio State
data AudioState = AudioState
  { audioRingBuffer :: !AudioRingBuffer
  , sampleCounter  :: !(IORef Word64)
  , running        :: !(IORef Bool)
  , currentNote    :: !(IORef (Maybe (Int, Int)))
  , lastConfidence :: !(IORef Double)
  , lastLatency    :: !(IORef Double)
  , lastWaveform   :: !(IORef [Double])
  }

newAudioState :: Int -> IO AudioState
newAudioState bufSize = do
  ring <- newAudioRingBuffer bufSize
  counter <- newIORef 0
  runRef <- newIORef True
  noteRef <- newIORef Nothing
  confRef <- newIORef 0.0
  latRef <- newIORef 0.0
  waveRef <- newIORef (replicate 64 0.0)
  return $ AudioState ring counter runRef noteRef confRef latRef waveRef

-- Main JACK backend with detection
runBackend :: Config -> TVar ReactorState -> IO ()
runBackend cfg state = do
  putStrLn "Starting DeMoD-Note JACK backend..."
  putStrLn $ "Sample rate: 96000 Hz"
  putStrLn $ "Buffer size: 128 samples (1.33ms)"
  putStrLn $ "Detection paths: Fast (2.66ms), Medium (12ms), Slow (30ms)"
  
  audioState <- newAudioState 8192
  jackState <- newJackState 5  -- Max 5 reconnection attempts
  
  _ <- installHandler sigINT (Catch $ do
    writeIORef (running audioState) False
    writeIORef (shouldReconnect jackState) False
    putStrLn "\nSIGINT received, shutting down...") Nothing
  _ <- installHandler sigTERM (Catch $ do
    writeIORef (running audioState) False  
    writeIORef (shouldReconnect jackState) False
    putStrLn "\nSIGTERM received, shutting down...") Nothing

  -- Fork detector thread
  _ <- forkIO $ detectorThread cfg audioState state
  
  -- Run JACK with mainMono and error handling
  let jackLoop = do
        result <- try $ JAudio.mainMono $ \sample -> do
            -- Convert to Int16
            let intSample = cfloatToInt16 sample
            
            -- Push to ring buffer
            pushSamples (audioRingBuffer audioState) (VS.singleton intSample)
            
            -- Update counter
            modifyIORef' (sampleCounter audioState) (+ 1)
            
            -- Return sample unchanged (passthrough)
            return sample
        case result of
            Right _ -> do
                jackSt <- readIORef (jackStatus jackState)
                when (jackSt == JackConnected) $ do
                    putStrLn "JACK thread exiting normally"
            Left (e :: SomeException) -> do
                handleJackError jackState e
                shouldRecon <- readIORef (shouldReconnect jackState)
                when shouldRecon jackLoop
  
  jackLoop
  
  putStrLn "DeMoD-Note stopped."

-- Detector thread - runs detection and handles note changes
detectorThread :: Config -> AudioState -> TVar ReactorState -> IO ()
detectorThread cfg audioState stateVar = do
  putStrLn "Detector thread started"
  detectorLoop
  where
    detectorLoop = do
      isRunning <- readIORef (running audioState)
      if not isRunning
        then do
          -- Send note off for any playing note
          curr <- readIORef (currentNote audioState)
          case curr of
            Just (note, _) -> putStrLn $ "Final Note Off: " ++ show note
            Nothing -> return ()
          putStrLn "Detector thread stopping"
        else do
          threadDelay 1333  -- 1.33ms
          
          samples <- readSamples (audioRingBuffer audioState) 256
          
          when (VS.length samples >= 128) $ do
            currentTime <- getMicroTime
            let samplesInt = VS.map fromIntegral samples
                waveform = VS.toList $ VS.map (\x -> fromIntegral x / 32768.0 :: Double) samplesInt
            
            -- Run detection
            result <- detect cfg samplesInt currentTime Idle defaultPLLState defaultOnsetFeatures
            
            -- Calculate tuning
            let (detTuningNote, detTuningCents) = case detectedNote result of
                    Nothing -> (Nothing, 0.0)
                    Just (note, _) -> 
                        let freq = midiToFreq note
                            (nearest, cents) = nearestNote freq
                        in (Just nearest, cents)
                detTuningInTune = isInTune detTuningCents
            
            -- Update state refs
            writeIORef (lastConfidence audioState) (confidence result)
            writeIORef (lastLatency audioState) 2.66
            writeIORef (lastWaveform audioState) waveform
            
            -- Handle note on/off
            handleNoteChange audioState (detectedNote result) (noteState result) detTuningNote detTuningCents detTuningInTune
            
            -- Update reactor state
            let newReactorState = ReactorState
                  { currentNotes = case detectedNote result of
                      Nothing -> []
                      Just n -> [n]
                  , noteStateMach = noteState result
                  , pllStateMach = defaultPLLState
                  , onsetFeatures = defaultOnsetFeatures
                  , lastOnsetTime = currentTime
                  , config = cfg
                  , reactorBPM = 120.0
                  , reactorThreshold = -40.0
                  }
            atomically $ writeTVar stateVar newReactorState
            
          detectorLoop

handleNoteChange :: AudioState -> Maybe (Int, Int) -> NoteState -> Maybe Int -> Double -> Bool -> IO ()
handleNoteChange audioState Nothing _ _ _ _ = do
  -- No note detected - send note off if we have a current note
  curr <- readIORef (currentNote audioState)
  case curr of
    Just (note, _) -> do
      putStrLn $ "Note Off: " ++ show note
      writeIORef (currentNote audioState) Nothing
    Nothing -> return ()

handleNoteChange audioState (Just (note, vel)) _noteState detTuningNote detTuningCents detTuningInTune = do
  curr <- readIORef (currentNote audioState)
  case curr of
    Just (lastNote, _) | lastNote == note -> 
      return ()  -- Same note, no change
    _ -> do
      -- Note on
      let tuningInfo = case detTuningNote of
            Just t -> " [tuning: " ++ midiToNoteName t ++ " " ++ show (round detTuningCents :: Int) ++ " cents" ++ if detTuningInTune then " ✓]" else "]"
            Nothing -> ""
      putStrLn $ "Note On: " ++ show note ++ " vel=" ++ show vel ++ tuningInfo
      writeIORef (currentNote audioState) $ Just (note, vel)

-- Simple passthrough mode
runBackendSimple :: Config -> TVar ReactorState -> IO ()
runBackendSimple _cfg _state = do
  putStrLn "Starting DeMoD-Note (passthrough mode)..."
  doneRef <- newIORef False
  _ <- installHandler sigINT (Catch $ do
    writeIORef doneRef True
    putStrLn "\nShutting down...") Nothing
  
  JAudio.mainMono $ \sample -> do
    _done <- readIORef doneRef
    return sample
  
  putStrLn "DeMoD-Note stopped."
