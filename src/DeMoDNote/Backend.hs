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
    newJackState
) where

import DeMoDNote.Types
import DeMoDNote.Config hiding (sampleRate, bufferSize)
import DeMoDNote.Detector
import DeMoDNote.OSC

import qualified Sound.JACK as JACK
import qualified Sound.JACK.Audio as JAudio
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM

import Foreign.C.Error (Errno(..))
import Foreign.C.Types (CFloat(..))
import Foreign.Marshal.Array (copyArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (sizeOf, peekElemOff)

import Control.Concurrent (threadDelay, forkIO, MVar, newEmptyMVar, tryTakeMVar, tryPutMVar)
import Control.Concurrent.STM
import Control.Exception (try, SomeException, catch, Exception)
import Control.Monad (forM, forM_, when, unless, forever)
import Data.Int (Int16)
import Data.IORef
import Data.Word (Word64)
import System.CPUTime (getCPUTime)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Posix.Signals (installHandler, sigINT, sigTERM, Handler(..))
import System.Process (callCommand, readProcess)

-------------------------------------------------------------------------------
-- Logging
-------------------------------------------------------------------------------

logInfo :: String -> IO ()
logInfo s = putStrLn s >> hFlush stdout

logErr :: String -> IO ()
logErr s = hPutStrLn stderr s >> hFlush stderr

-------------------------------------------------------------------------------
-- JackState (uses JackStatus from Types.hs)
-------------------------------------------------------------------------------

data JackState = JackState
    { jsStatus             :: !(IORef JackStatus)
    , jsReconnectAttempts  :: !(IORef Int)
    , jsMaxReconnectAttempts :: !Int
    , jsShouldReconnect    :: !(IORef Bool)
    }

newJackState :: Int -> IO JackState
newJackState maxAttempts =
    JackState
        <$> newIORef JackDisconnected
        <*> newIORef 0
        <*> pure maxAttempts
        <*> newIORef True

-------------------------------------------------------------------------------
-- JackException
-------------------------------------------------------------------------------

data JackException
    = JackConnectionFailed String
    | JackDisconnectedErr  String
    | JackReconnectFailed  Int
    | JackClientError      String
    deriving (Show)

instance Exception JackException

-------------------------------------------------------------------------------
-- DetectionEvent
-------------------------------------------------------------------------------

data DetectionEvent = DetectionEvent
    { deNote         :: Maybe (Int, Int)
    , deConfidence   :: Double
    , deLatency      :: Double
    , deWaveform     :: [Double]
    , deState        :: NoteState
    , deTuningNote   :: Maybe Int
    , deTuningCents  :: Double
    , deTuningInTune :: Bool
    , deJackStatus   :: JackStatus
    }

-------------------------------------------------------------------------------
-- JACK Server Management
-------------------------------------------------------------------------------

isJACKRunning :: IO Bool
isJACKRunning = do
    result <- try (readProcess "jack_control" ["status"] "") :: IO (Either SomeException String)
    case result of
        Right output -> return $ "running" `elem` words output
        Left _ -> do
            result2 <- try (readProcess "pgrep" ["jackd"] "") :: IO (Either SomeException String)
            case result2 of
                Right _ -> return True
                Left _ -> return False

startJACKServer :: IO Bool
startJACKServer = do
    putStrLn "Attempting to start JACK server..."
    result <- try (callCommand "jackd -d dummy -r 44100 -p 256 -n 2 2>/dev/null &") :: IO (Either SomeException ())
    case result of
        Left e  -> logErr ("Failed: " ++ show e) >> return False
        Right _ -> do
            threadDelay 3000000
            ok <- isJACKRunning
            if ok then logInfo "JACK server started." >> return True
                  else logErr  "JACK server failed to start." >> return False

-------------------------------------------------------------------------------
-- Reconnection with Exponential Backoff
-------------------------------------------------------------------------------

reconnectBackoff :: Int -> IO ()
reconnectBackoff n = do
    let µs = min 16000000 (2 ^ n * 1000000)
    logInfo $ "Backoff: waiting " ++ show (µs `div` 1000000) ++ " s..."
    threadDelay µs

attemptReconnect :: JackState -> IO Bool
attemptReconnect st = do
    attempts <- readIORef (jsReconnectAttempts st)
    let maxAttempts = jsMaxReconnectAttempts st
    if attempts >= maxAttempts
        then do
            logErr "Max reconnection attempts reached"
            return False
        else do
            logInfo $ "Reconnection attempt " ++ show (attempts + 1) ++ "..."
            modifyIORef' (jsReconnectAttempts st) (+ 1)
            reconnectBackoff attempts
            return True

-------------------------------------------------------------------------------
-- Time
-------------------------------------------------------------------------------

getMicroTime :: IO Word64
getMicroTime = fromIntegral . (`div` 1000000) <$> getCPUTime

-------------------------------------------------------------------------------
-- Audio Ring Buffer
-------------------------------------------------------------------------------

data AudioRingBuffer = AudioRingBuffer
    { rbMVec   :: !(VSM.IOVector Int16)
    , rbWritePos :: !(IORef Int)
    , rbSize   :: !Int
    }

newAudioRingBuffer :: Int -> IO AudioRingBuffer
newAudioRingBuffer size = do
    mvec <- VSM.new size
    wp <- newIORef 0
    return $ AudioRingBuffer mvec wp size

writeFloat2Int16Frame :: VSM.IOVector Int16 -> Int -> Int -> Ptr CFloat -> Int -> IO Int
writeFloat2Int16Frame mvec wp size ptr n = do
    let remaining = size - wp
        toWrite = min n remaining
    forM_ [0..toWrite-1] $ \i -> do
        val <- peekElemOff ptr i :: IO CFloat
        let intVal = cfloatToInt16 val
        VSM.write mvec (wp + i) intVal
    let newWp = wp + toWrite
    return newWp

-- Write audio samples from JACK callback to ring buffer and signal detector thread
writeToRingBuffer :: AudioState -> Ptr CFloat -> Int -> IO ()
writeToRingBuffer audioState buf n = do
    let rb = audioRingBuffer audioState
        mvec = rbMVec rb
        size = rbSize rb
    
    wp <- readIORef (rbWritePos rb)
    newWp <- writeFloat2Int16Frame mvec wp size buf n
    
    -- Update write position
    writeIORef (rbWritePos rb) (newWp `mod` size)
    
    -- Signal detector thread that new data is available
    _ <- tryPutMVar (rbSemaphore audioState) ()
    return ()

-------------------------------------------------------------------------------
-- AudioState
-------------------------------------------------------------------------------

data AudioState = AudioState
    { audioRingBuffer :: !AudioRingBuffer
    , sampleCounter   :: !(TVar Word64)
    , running         :: !(TVar Bool)
    , currentNote     :: !(TVar (Maybe (Int, Int)))
    , lastConfidence  :: !(TVar Double)
    , lastLatency     :: !(TVar Double)
    , lastWaveform    :: !(TVar [Double])
    , oscClient       :: !(TVar (Maybe OscClient))
    , sampleRate      :: !(TVar Int)
    , bufferSize      :: !(TVar Int)
    , xrunCount       :: !(IORef Word64)
    , rbSemaphore     :: !(MVar ())
    }

newAudioState :: Int -> IO AudioState
newAudioState ringSize = do
    rb <- newAudioRingBuffer ringSize
    counter <- newTVarIO 0
    run <- newTVarIO True
    note <- newTVarIO Nothing
    conf <- newTVarIO 0.0
    lat <- newTVarIO 0.0
    wave <- newTVarIO []
    osc <- newTVarIO Nothing
    sr <- newTVarIO 44100
    bs <- newTVarIO 256
    xruns <- newIORef 0
    sem <- newEmptyMVar
    return $ AudioState rb counter run note conf lat wave osc sr bs xruns sem

cfloatToInt16 :: CFloat -> Int16
cfloatToInt16 (CFloat f) = round (max (-1.0) (min 1.0 f) * 32767)

-------------------------------------------------------------------------------
-- Real-time Process Callback (Exact jack-0.7.2.2 signature + NFrames unwrap)
-------------------------------------------------------------------------------

jackProcessCallback
    :: AudioState
    -> JACK.Port JAudio.Sample JACK.Input
    -> JACK.Port JAudio.Sample JACK.Output
    -> JACK.NFrames
    -> Sync.ExceptionalT Errno IO ()
jackProcessCallback audioState inPort outPort nframes = Trans.lift go
  where
    go = do
        let JACK.NFrames w = nframes          -- unwrap newtype (Word32)
            n = fromIntegral w :: Int
        
        inPtr <- JAudio.getBufferPtr inPort nframes
        writeToRingBuffer audioState (castPtr inPtr) n
        
        outPtr <- JAudio.getBufferPtr outPort nframes
        copyArray outPtr (castPtr inPtr) (n * sizeOf (undefined :: CFloat))
        
        return ()

-------------------------------------------------------------------------------
-- Clean control loop (plain IO)
-------------------------------------------------------------------------------

jackControlLoop :: AudioState -> IO ()
jackControlLoop audioState = loop
  where
    loop = do
        isRunning <- atomically $ readTVar (running audioState)
        if isRunning
            then threadDelay 100000 >> loop
            else logInfo "Shutdown requested - exiting JACK session"

-------------------------------------------------------------------------------
-- One full JACK lifetime (withClientDefault + withPort + withProcess)
-------------------------------------------------------------------------------

jackSession
    :: Config
    -> AudioState
    -> JackState
    -> TVar ReactorState
    -> IO ()
jackSession cfg audioState jackState stateVar = do
    logInfo "Starting JACK client..."

    -- Detector thread lives for the lifetime of this JACK session
    _ <- forkIO $ detectorThread cfg audioState jackState stateVar

    JACK.handleExceptions $
        JACK.withClientDefault "DeMoDNote" $ \client ->
            JACK.withPort client "input" $ \inPort ->
                JACK.withPort client "output" $ \outPort ->
                    JACK.withProcess client (jackProcessCallback audioState inPort outPort) $
                        JACK.withActivation client $ do
                            -- All IO actions lifted into ExceptionalT context
                            sr <- Trans.lift $ JACK.getSampleRate client
                            bs <- Trans.lift $ JACK.getBufferSize client
                            
                            Trans.lift $ atomically $ writeTVar (sampleRate audioState) sr
                            Trans.lift $ atomically $ writeTVar (bufferSize audioState) bs
                            Trans.lift $ writeIORef (jsStatus jackState) JackConnected
                            
                            Trans.lift $ logInfo $ "✅ JACK connected (SR=" ++ show sr ++ ", BS=" ++ show bs ++ ")"
                            
                            Trans.lift $ jackControlLoop audioState

    logInfo "JACK session ended cleanly"

-------------------------------------------------------------------------------
-- Reconnection Loop
-------------------------------------------------------------------------------

jackLoop :: Config -> AudioState -> JackState -> TVar ReactorState -> IO ()
jackLoop cfg audioState jackState stateVar = do
    recon    <- readIORef (jsShouldReconnect jackState)
    running' <- atomically $ readTVar (running audioState)
    when (recon && running') $ do
        catch (jackSession cfg audioState jackState stateVar) 
              (\(e :: SomeException) -> do
                logErr $ "JACK session lost: " ++ show e
                writeIORef (jsStatus jackState) (JackError "session lost")
                ok <- attemptReconnect jackState
                when ok $ jackLoop cfg audioState jackState stateVar)

-------------------------------------------------------------------------------
-- Detector Thread (exact original)
-------------------------------------------------------------------------------

detectorThread :: Config -> AudioState -> JackState -> TVar ReactorState -> IO ()
detectorThread cfg audioState jackState stateVar = do
    logInfo "Detector thread started."
    loop (0 :: Int)
  where
    rb = audioRingBuffer audioState
    
    loop !silentTicks = do
        isRunning <- atomically $ readTVar (running audioState)
        unless isRunning $ do
            sendFinalNoteOff audioState
            logInfo "Detector thread stopping."
            return ()
        
        when isRunning $ do
            mReady <- tryTakeMVar (rbSemaphore audioState)
            case mReady of
                Nothing -> do
                    threadDelay 10000
                    xruns <- readIORef (xrunCount audioState)
                    when (silentTicks > 200) $ do
                        logErr $ "[Detector] No JACK frames for ~2s (xruns=" ++ show xruns ++ ")"
                    loop (silentTicks + 1)
                Just () -> do
                    processFrame cfg audioState jackState stateVar
                    loop 0

sendFinalNoteOff :: AudioState -> IO ()
sendFinalNoteOff audioState = do
    curr <- atomically $ readTVar (currentNote audioState)
    case curr of
        Just (note, _) -> do
            mClient <- atomically $ readTVar (oscClient audioState)
            case mClient of
                Just client -> sendNoteOff client note `catch` (\(_ :: SomeException) -> return ())
                Nothing -> return ()
        Nothing -> return ()

processFrame :: Config -> AudioState -> JackState -> TVar ReactorState -> IO ()
processFrame cfg audioState jackState stateVar = do
    let rb = audioRingBuffer audioState
    wp <- readIORef (rbWritePos rb)
    let size = rbSize rb
    
    -- Read last 256 samples
    let startPos = if wp >= 256 then wp - 256 else size - (256 - wp)
    samples <- forM [0..255] $ \i -> VSM.read (rbMVec rb) ((startPos + i) `mod` size)
    let samplesVec = VS.fromList samples :: VS.Vector Int16
    
    when (VS.length samplesVec >= 128) $ do
        currentTime <- getMicroTime
        
        let !samplesInt = VS.map fromIntegral samplesVec
            !samplesD = VS.map (\x -> fromIntegral x / 32768.0 :: Double) samplesInt
            !waveform = VS.toList samplesD
        
        result <- detect cfg samplesInt currentTime Idle defaultPLLState defaultOnsetFeatures
        
        let (detTuningNote, detTuningCents) = case detectedNote result of
                Nothing        -> (Nothing, 0.0)
                Just (note, _) ->
                    let (nearest, cents) = nearestNote (midiToFreq note)
                    in  (Just nearest, cents)
            detTuningInTune = isInTune detTuningCents
        
        atomically $ writeTVar (lastConfidence audioState) (confidence result)
        atomically $ writeTVar (lastLatency audioState) 2.66
        atomically $ writeTVar (lastWaveform audioState) waveform
        
        handleNoteChange audioState (detectedNote result) (noteState result) detTuningNote detTuningCents detTuningInTune
        
        -- Get current JACK status from the IORef
        jackSt <- readIORef (jsStatus jackState)
        
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
              -- New fields for TUI integration
              , jackStatus = jackSt
              , detectionConfidence = confidence result
              , detectionLatency = 2.66
              , latestWaveform = waveform
              , detectedTuningNote = detTuningNote
              , detectedTuningCents = detTuningCents
              , detectedTuningInTune = detTuningInTune
              }
        atomically $ writeTVar stateVar newReactorState

handleNoteChange :: AudioState -> Maybe (MIDINote, Velocity) -> NoteState -> Maybe Int -> Double -> Bool -> IO ()
handleNoteChange audioState mNote noteState detTuningNote detTuningCents detTuningInTune = do
    curr <- atomically $ readTVar (currentNote audioState)
    case (curr, mNote) of
        (Nothing, Just (note, vel)) -> do
            logInfo $ "Note On: " ++ show note
            mClient <- atomically $ readTVar (oscClient audioState)
            case mClient of
                Just client -> sendNoteOn client note vel `catch` (\(_ :: SomeException) -> return ())
                Nothing -> return ()
            atomically $ writeTVar (currentNote audioState) $ Just (note, vel)
        (Just (note, _), Nothing) -> do
            logInfo $ "Note Off: " ++ show note
            mClient <- atomically $ readTVar (oscClient audioState)
            case mClient of
                Just client -> sendNoteOff client note `catch` (\(_ :: SomeException) -> return ())
                Nothing -> return ()
            atomically $ writeTVar (currentNote audioState) Nothing
        (Just (oldNote, _), Just (newNote, vel)) | newNote /= oldNote -> do
            logInfo $ "Note Off: " ++ show oldNote
            mClient <- atomically $ readTVar (oscClient audioState)
            case mClient of
                Just client -> sendNoteOff client oldNote `catch` (\(_ :: SomeException) -> return ())
                Nothing -> return ()
            logInfo $ "Note On: " ++ show newNote
            case mClient of
                Just client -> sendNoteOn client newNote vel `catch` (\(_ :: SomeException) -> return ())
                Nothing -> return ()
            atomically $ writeTVar (currentNote audioState) $ Just (newNote, vel)
        _ -> return ()

-------------------------------------------------------------------------------
-- Main Backend Entry Points
-------------------------------------------------------------------------------

runBackend :: Config -> TVar ReactorState -> IO ()
runBackend cfg state = do
    logInfo "Starting DeMoD-Note JACK backend..."
    
    started <- startJACKServer
    when started $ logInfo "JACK server started"
    
    audioState <- newAudioState 8192
    jackState <- newJackState 5
    
    mOscClient <- (do
        client <- createOscClient "127.0.0.1" 57120
        logInfo "OSC client connected"
        return $ Just client) `catch` (\e -> do
        logErr $ "Could not create OSC client: " ++ show (e :: SomeException)
        return Nothing)
    atomically $ writeTVar (oscClient audioState) mOscClient
    
    _ <- installHandler sigINT (Catch $ do
        atomically $ writeTVar (running audioState) False
        writeIORef (jsShouldReconnect jackState) False
        logInfo "SIGINT received, shutting down...") Nothing
    _ <- installHandler sigTERM (Catch $ do
        atomically $ writeTVar (running audioState) False  
        writeIORef (jsShouldReconnect jackState) False
        logInfo "SIGTERM received, shutting down...") Nothing
    
    forkIO $ jackLoop cfg audioState jackState state
    
    logInfo "DeMoD-Note JACK backend started successfully."
    
    forever $ threadDelay 1000000

runBackendSimple :: Config -> TVar ReactorState -> IO ()
runBackendSimple _cfg _state = do
    logInfo "Starting DeMoDNote (passthrough mode)..."
    logInfo "Note: JACK server must be running with input/output ports"
    logInfo "Use: jackd -d dummy -r 44100 -p 256"
    logInfo "Then connect: jack_connect system:capture_1 DeMoDNote:input"
    logInfo "         jack_connect DeMoDNote:output system:playback_1"
    logInfo "DeMoDNote stopped."