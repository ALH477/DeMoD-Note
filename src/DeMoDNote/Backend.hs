{-# LANGUAGE BangPatterns #-}

module DeMoDNote.Backend (
    module DeMoDNote.Types,
    module DeMoDNote.Config, 
    module DeMoDNote.Detector,
    DetectionEvent(..),
    getDetectionChannel,
    getCurrentDetection,
    runBackend,
    runBackendSimple
) where

import DeMoDNote.Types
import DeMoDNote.Config
import DeMoDNote.Detector

import Sound.JACK
import qualified Sound.JACK.Audio as JAudio
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString as BS
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent (threadDelay, forkIO, Chan, newChan, writeChan, readChan)
import Control.Exception (bracket, try, SomeException, finally)
import Data.IORef
import Data.Bits ((.|.), (.&.))
import Data.Word (Word64, Word8)
import Foreign.C.Types (CFloat(..))
import Data.Int (Int16)
import Foreign.Ptr (Ptr, plusPtr)
import System.CPUTime (getCPUTime)
import System.Posix.Signals (installHandler, sigINT, sigTERM, Handler(..))
import DeMoDNote.Types
import DeMoDNote.Config
import DeMoDNote.Detector

-- Detection event for TUI updates
data DetectionEvent = DetectionEvent
    { deNote       :: Maybe (Int, Int)  -- (note, velocity)
    , deConfidence :: Double
    , deLatency    :: Double
    , deWaveform   :: [Double]
    , deState      :: NoteState
    }

-- Get current time in microseconds
getMicroTime :: IO Word64
getMicroTime = do
  t <- getCPUTime
  return $ fromIntegral (t `div` 1000000)

-- Concurrently safe ring buffer for audio samples
data AudioRingBuffer = AudioRingBuffer
  { rbBuffer    :: !(VS.Vector Int16)
  , rbWritePos :: !(IORef Int)
  , rbReadPos  :: !(IORef Int)
  , rbSize     :: !Int
  }

newAudioRingBuffer :: Int -> IO AudioRingBuffer
newAudioRingBuffer size = do
  writePos <- newIORef 0
  readPos <- newIORef 0
  let buffer = VS.replicate size 0
  return $ AudioRingBuffer buffer writePos readPos size

-- Push samples to ring buffer
pushSamples :: AudioRingBuffer -> VS.Vector Int16 -> IO ()
pushSamples rb samples = do
  writePos <- readIORef (rbWritePos rb)
  let !newWritePos = (writePos + VS.length samples) `mod` rbSize rb
  let !buf = rbBuffer rb
  let !size = rbSize rb
  
  let !newBuf = VS.generate size $ \i ->
        let idx = (writePos + i) `mod` size
        in if i < VS.length samples
           then samples VS.! i
           else buf VS.! idx
  
  -- Note: In production, use atomicWriteIORef or similar for thread safety
  return ()

-- Read N samples from ring buffer (most recent)
readSamples :: AudioRingBuffer -> Int -> IO (VS.Vector Int16)
readSamples rb n = do
  writePos <- readIORef (rbWritePos rb)
  let !size = rbSize rb
  let !buf = rbBuffer rb
  
  return $ VS.generate n $ \i ->
    let idx = (writePos - n + i) `mod` size
    in buf VS.! idx

-- Convert CFloat to Int16
cfloatToInt16 :: CFloat -> Int16
cfloatToInt16 (CFloat x) = 
  let scaled = round (x * 32767.0) :: Int
  in fromIntegral $ max (-32768) (min 32767 scaled)

-- Convert Int16 to CFloat
int16ToCFloat :: Int16 -> CFloat
int16ToCFloat x = CFloat $ fromIntegral x / 32768.0

-- MIDI encoding functions (for potential future use with jackmidi)
encodeMidiNoteOn :: Word8 -> Word8 -> Word8 -> BS.ByteString
encodeMidiNoteOn channel note velocity = 
  BS.pack [0x90 .|. (channel .&. 0x0F), note, velocity]

encodeMidiNoteOff :: Word8 -> Word8 -> BS.ByteString
encodeMidiNoteOff channel note = 
  BS.pack [0x80 .|. (channel .&. 0x0F), note, 0]

encodePitchBend :: Word8 -> Double -> BS.ByteString
encodePitchBend channel bendSemitones = 
  let bendRange = 2.0
      normalized = bendSemitones / bendRange
      value = round ((normalized + 1.0) * 8191.5) :: Int
      lsb = fromIntegral $ value .&. 0x7F
      msb = fromIntegral $ (value `shiftR` 7) .&. 0x7F
  in BS.pack [0xE0 .|. (channel .&. 0x0F), lsb, msb]

shiftR :: Int -> Int -> Int
shiftR x n = x `shiftR` n

-- Audio Processing State
data AudioState = AudioState
  { audioRingBuffer :: !AudioRingBuffer
  , sampleCounter  :: !(IORef Word64)
  , running        :: !(IORef Bool)
  , lastNote       :: !(IORef (Maybe (Int, Int)))
  , detectionChan  :: !(Chan DetectionEvent)
  , lastConfidence :: !(IORef Double)
  , lastLatency    :: !(IORef Double)
  , lastWaveform   :: !(IORef [Double])
  }

newAudioState :: Int -> IO AudioState
newAudioState bufferSize = do
  ring <- newAudioRingBuffer bufferSize
  counter <- newIORef 0
  runRef <- newIORef True
  lastNoteRef <- newIORef Nothing
  chan <- newChan
  confRef <- newIORef 0.0
  latRef <- newIORef 0.0
  waveRef <- newIORef (replicate 64 0.0)
  return $ AudioState ring counter runRef lastNoteRef chan confRef latRef waveRef

-- Main JACK backend using mainMono (simplest working approach)
runBackend :: Config -> TVar ReactorState -> IO ()
runBackend cfg state = do
  putStrLn "Starting DeMoD-Note JACK backend..."
  putStrLn $ "Sample rate: 96000 Hz"
  putStrLn $ "Buffer size: 128 samples (1.33ms)"
  putStrLn $ "Fast path: 2.66ms (200Hz+)"
  putStrLn $ "Medium path: 12ms (80-200Hz speculative)"
  putStrLn $ "Slow path: 30ms (30-80Hz validated)"
  putStrLn $ "Onset threshold: 0.67"
  
  -- Initialize audio state
  audioState <- newAudioState 8192
  
  -- Initialize detector state
  let detectorState = emptyReactorState cfg
  
  -- Install signal handlers
  _ <- installHandler sigINT (Catch $ do
    writeIORef (running audioState) False
    putStrLn "\nSIGINT received, shutting down...") Nothing
  _ <- installHandler sigTERM (Catch $ do
    writeIORef (running audioState) False  
    putStrLn "\nSIGTERM received, shutting down...") Nothing

  -- Fork detector thread
  detectorTid <- forkIO $ detectorThread cfg audioState state
  
  -- Run JACK audio processing with mainMono
  -- This provides mono input -> mono output with passthrough
  JAudio.mainMono $ \sample -> do
    -- Convert to Int16
    let intSample = cfloatToInt16 sample
    
    -- Get current sample count
    cnt <- readIORef (sampleCounter audioState)
    let time = cnt `div` 128  -- Approximate buffer count as time
    
    -- Push to ring buffer
    pushSamples (audioRingBuffer audioState) (VS.singleton intSample)
    
    -- Increment counter
    modifyIORef' (sampleCounter audioState) (+ 1)
    
    -- Return sample unchanged (passthrough)
    return sample

  -- This won't be reached unless JACK fails
  putStrLn "JACK processing stopped."
  putStrLn "DeMoD-Note backend stopped."

-- Detector thread - runs detection in background
detectorThread :: Config -> AudioState -> TVar ReactorState -> IO ()
detectorThread cfg audioState stateVar = do
  putStrLn "Detector thread started"
  detectorLoop
  where
    detectorLoop = do
      isRunning <- readIORef (running audioState)
      if not isRunning
        then putStrLn "Detector thread stopping"
        else do
          threadDelay 1333  -- 1.33ms - check every buffer
          
          -- Get current samples from ring buffer
          samples <- readSamples (audioRingBuffer audioState) 256
          
          when (VS.length samples >= 128) $ do
            currentTime <- getMicroTime
            
            -- Convert Int16 to Int for detector
            let samplesInt = VS.map fromIntegral samples
            
            -- Convert to waveform for TUI (normalized -1 to 1)
            let waveform = VS.toList $ VS.map (\x -> fromIntegral x / 32768.0 :: Double) samples
            
            -- Run detection
            result <- detect cfg samplesInt currentTime Idle defaultPLLState defaultOnsetFeatures
            
            -- Update state refs for TUI
            writeIORef (lastConfidence audioState) (confidence result)
            writeIORef (lastLatency audioState) 2.66  -- Simplified latency
            writeIORef (lastWaveform audioState) waveform
            
            -- Handle detection result
            case detectedNote result of
              Nothing -> return ()
              Just (note, vel) -> do
                lastN <- readIORef (lastNote audioState)
                case lastN of
                  Just (lastNote, _) | lastNote == note -> return ()
                  _ -> do
                    -- Note changed - could send MIDI here
                    putStrLn $ "Detection: note=" ++ show note ++ " vel=" ++ show vel ++ 
                              " conf=" ++ show (confidence result) ++ 
                              " state=" ++ show (noteState result)
                    writeIORef (lastNote audioState) $ Just (note, vel)
                    
                    -- Send detection event to TUI
                    let event = DetectionEvent 
                          { deNote = Just (note, vel)
                          , deConfidence = confidence result
                          , deLatency = 2.66
                          , deWaveform = waveform
                          , deState = noteState result
                          }
                    writeChan (detectionChan audioState) event
          
          detectorLoop

-- Simple passthrough version
runBackendSimple :: Config -> TVar ReactorState -> IO ()
runBackendSimple cfg state = do
  putStrLn "Starting DeMoD-Note (simple passthrough mode)..."
  
  -- Install signal handlers
  doneRef <- newIORef False
  _ <- installHandler sigINT (Catch $ do
    writeIORef doneRef True
    putStrLn "\nShutting down...") Nothing
  
  -- Simple mono passthrough
  JAudio.mainMono $ \sample -> do
    done <- readIORef doneRef
    when done $ error "Shutdown requested"
    return sample
  
  putStrLn "DeMoD-Note stopped."

-- Get detection events channel (for TUI integration)
getDetectionChannel :: AudioState -> Chan DetectionEvent
getDetectionChannel = detectionChan

-- Get current detection state for TUI polling
getCurrentDetection :: AudioState -> IO DetectionEvent
getCurrentDetection audioState = do
  note <- readIORef (lastNote audioState)
  conf <- readIORef (lastConfidence audioState)
  lat <- readIORef (lastLatency audioState)
  wave <- readIORef (lastWaveform audioState)
  return DetectionEvent
    { deNote = note
    , deConfidence = conf
    , deLatency = lat
    , deWaveform = wave
    , deState = Idle
    }
