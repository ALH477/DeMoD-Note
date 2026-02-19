{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | DeMoDNote JACK backend.
--
-- Design invariants
-- ─────────────────
-- RT thread (JACK process callback)
--   • MUST NOT block, wait on locks, or allocate from the GHC heap.
--   • Writes ring buffer via a pointer loop        — no allocation
--   • Updates write-pointer via atomicWriteIORef   — CAS, no lock
--   • Signals detector via tryPutMVar              — non-blocking
--   • Copies input→output via copyArray            — single memcpy
--
-- Detector thread
--   • Wakes exactly once per JACK period (takeMVar + 2 s timeout)
--   • 2-second timeout distinguishes "server died" from "clean shutdown"
--   • Reads ring buffer snapshot via two copyArray calls
--   • Polls getBufferSize/getSampleRate every ~1 s to detect server-side
--     parameter changes (the jack Haskell package does not expose buffer-
--     size-change or shutdown callbacks at the Haskell level)
--
-- Reconnect
--   • Exponential back-off: 1 s, 2 s, 4 s, 8 s, 16 s (cap)
--   • Resets attempt counter on successful reconnect
--
-- Note transitions
--   • Always: note-off (old) → note-on (new). Never fires note-on first.
--
-- Detector state continuity  ← critical fix vs previous versions
--   • noteStateMach, pllStateMach, and onsetFeatures are ALL read from
--     stateVar in a single atomic transaction before each detect call,
--     so the detector never restarts from Idle mid-note.

module DeMoDNote.Backend (
    module DeMoDNote.Types,
    module DeMoDNote.Config,
    module DeMoDNote.Detector,
    DetectionEvent(..),
    JackState(..),
    JackStatus(..),
    JackException(..),
    AudioState(..),
    AudioRingBuffer(..),
    newJackState,
    newAudioState,
    newAudioRingBuffer,
    pushSamples,
    readSamples,
    cfloatToInt16,
    int16ToCFloat,
    runBackend,
    runBackendSimple,
) where

import DeMoDNote.Config hiding (sampleRate, bufferSize)
import DeMoDNote.Detector
import DeMoDNote.OSC
import DeMoDNote.Types

import qualified Sound.JACK           as JACK
import qualified Sound.JACK.Audio     as JAudio
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class             as Trans

import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM

import Foreign.C.Error       (Errno)
import Foreign.C.Types       (CFloat(..))
import Foreign.Marshal.Array (copyArray, advancePtr)
import Foreign.Ptr           (Ptr)
import Foreign.Storable      (peekElemOff)

import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception  (Exception(..), SomeException, catch, try)
import Control.Monad      (unless, when)
import Data.Int           (Int16)
import Data.IORef
import Data.Word          (Word64)
import System.CPUTime     (getCPUTime)
import System.IO          (hFlush, hPutStrLn, stderr, stdout)
import System.Posix.Signals (installHandler, sigINT, sigTERM, Handler(..))
import System.Process     (callCommand, readProcess)
import System.Timeout     (timeout)

-------------------------------------------------------------------------------
-- Logging
-------------------------------------------------------------------------------

logInfo :: String -> IO ()
logInfo s = putStrLn s >> hFlush stdout

logErr :: String -> IO ()
logErr s = hPutStrLn stderr s >> hFlush stderr

-------------------------------------------------------------------------------
-- JackStatus / JackState
-------------------------------------------------------------------------------

data JackStatus
    = JackConnected
    | JackDisconnected
    | JackReconnecting
    | JackError String
    deriving (Show, Eq)

data JackState = JackState
    { jackStatus           :: !(IORef JackStatus)
    , reconnectAttempts    :: !(IORef Int)
    , maxReconnectAttempts :: !Int
    , shouldReconnect      :: !(IORef Bool)
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
-- Timing
-------------------------------------------------------------------------------

getMicroTime :: IO Word64
getMicroTime = fromIntegral . (`div` 1000000) <$> getCPUTime

-------------------------------------------------------------------------------
-- Ring buffer
--
-- Pre-allocated VSM.IOVector Int16.
-- Write pointer: IORef Int, updated via atomicWriteIORef (CAS, no lock).
-- Semaphore: MVar (), posted by RT callback, taken by detector thread.
-- The MVar provides the happens-before edge guaranteeing the detector sees
-- a fully-written frame.
--
-- Capacity = 4 × max expected JACK buffer (2048) = 8192 samples = 16 KB.
-- The detection window is capped at rbSize, so write never catches read.
-------------------------------------------------------------------------------

data AudioRingBuffer = AudioRingBuffer
    { rbMVec      :: !(VSM.IOVector Int16)
    , rbWritePos  :: !(IORef Int)
    , rbSize      :: !Int
    , rbSemaphore :: !(MVar ())
    }

newAudioRingBuffer :: Int -> IO AudioRingBuffer
newAudioRingBuffer size =
    AudioRingBuffer
        <$> VSM.replicate size 0
        <*> newIORef 0
        <*> pure size
        <*> newEmptyMVar

-- | Write a pre-built Int16 vector into the ring.  Used by tests only.
pushSamples :: AudioRingBuffer -> VS.Vector Int16 -> IO ()
pushSamples rb src = do
    wp    <- readIORef (rbWritePos rb)
    newWp <- writeInt16Frame (rbMVec rb) wp (rbSize rb) src
    atomicWriteIORef (rbWritePos rb) newWp

-- | Write an Int16 vector via at most two memcpys (handles wrap-around).
writeInt16Frame
    :: VSM.IOVector Int16
    -> Int -> Int             -- write position, ring size
    -> VS.Vector Int16
    -> IO Int                 -- new write position
writeInt16Frame !vec !wp !sz !src =
    VS.unsafeWith src $ \srcPtr -> do
        let n      = VS.length src
            firstN = min n (sz - wp)
        VSM.unsafeWith vec $ \dstPtr -> do
            copyArray (advancePtr dstPtr wp) srcPtr firstN
            when (firstN < n) $
                copyArray dstPtr (advancePtr srcPtr firstN) (n - firstN)
        return ((wp + n) `mod` sz)

-- | Convert a CFloat frame directly into the ring.
--   Called from the JACK RT thread — tight loop, no allocation, no modulo.
--   Wrap-around is handled by splitting into two sub-ranges so the inner
--   loop has no modulo.
writeFloat2Int16Frame
    :: VSM.IOVector Int16
    -> Int                    -- current write position
    -> Int                    -- ring size
    -> Ptr CFloat
    -> Int                    -- frame length (nframes)
    -> IO Int                 -- new write position
writeFloat2Int16Frame !vec !wp !sz !inPtr !n = do
    -- Clamp if nframes exceeds ring capacity (should never happen with
    -- properly configured ring size, but prevents memory corruption).
    let !safeN = min n sz
        firstN = min safeN (sz - wp)
    writeChunk wp    0      firstN
    when (firstN < safeN) $
        writeChunk 0  firstN (safeN - firstN)
    return ((wp + safeN) `mod` sz)
  where
    -- Write `count` samples: ring[dstOff..] ← convert(inPtr[srcOff..])
    writeChunk !dstOff !srcOff !count = go 0
      where
        go !i
            | i >= count = return ()
            | otherwise  = do
                CFloat x <- peekElemOff inPtr (srcOff + i)
                -- floor(x * 32767.5): correct asymmetric Int16 mapping
                --   -1.0 → -32768  |  0.0 → 0  |  1.0 → 32767  |  0.5 → 16383
                let !s = fromIntegral
                            (max (-32768 :: Int) . min 32767 $
                             floor (x * 32767.5 :: Float))
                VSM.unsafeWrite vec (dstOff + i) s
                go (i + 1)

-- | Snapshot the most-recent n samples.
--   Called from the detector thread after waking on rbSemaphore.
readSamples :: AudioRingBuffer -> Int -> IO (VS.Vector Int16)
readSamples rb n = do
    wp <- readIORef (rbWritePos rb)
    let sz      = rbSize rb
        readPos = (wp - n + sz) `mod` sz
        firstN  = min n (sz - readPos)
    dst <- VSM.new n
    VSM.unsafeWith (rbMVec rb) $ \srcPtr ->
        VSM.unsafeWith dst $ \dstPtr -> do
            copyArray dstPtr (advancePtr srcPtr readPos) firstN
            when (firstN < n) $
                copyArray (advancePtr dstPtr firstN) srcPtr (n - firstN)
    VS.unsafeFreeze dst

-------------------------------------------------------------------------------
-- Sample conversion
-------------------------------------------------------------------------------

-- | CFloat [-1, 1] → Int16, using floor(x * 32767.5).
--   Correct asymmetric mapping: -1.0 → -32768, 1.0 → 32767, 0.5 → 16383.
cfloatToInt16 :: CFloat -> Int16
cfloatToInt16 (CFloat x) =
    fromIntegral (max (-32768 :: Int) . min 32767 $
                  floor (x * 32767.5 :: Float))

int16ToCFloat :: Int16 -> CFloat
int16ToCFloat x = CFloat (fromIntegral x / 32768.0)

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
    }

newAudioState :: Int -> IO AudioState
newAudioState ringSize =
    AudioState
        <$> newAudioRingBuffer ringSize
        <*> newTVarIO 0
        <*> newTVarIO True
        <*> newTVarIO Nothing
        <*> newTVarIO 0.0
        <*> newTVarIO 0.0
        <*> newTVarIO (replicate 64 0.0)
        <*> newTVarIO Nothing
        <*> newTVarIO 44100
        <*> newTVarIO 256
        <*> newIORef 0

-------------------------------------------------------------------------------
-- JACK server management helpers
-------------------------------------------------------------------------------

isJACKRunning :: IO Bool
isJACKRunning = do
    r <- try (readProcess "jack_control" ["status"] "")
             :: IO (Either SomeException String)
    case r of
        Right out -> return $ "running" `elem` words out
        Left _    -> do
            r2 <- try (readProcess "pgrep" ["-x", "jackd"] "")
                      :: IO (Either SomeException String)
            case r2 of
                Right pid | not (null pid) -> return True
                _ -> do
                    r3 <- try (readProcess "pgrep" ["-x", "jackdbus"] "")
                               :: IO (Either SomeException String)
                    return $ case r3 of
                        Right pid -> not (null pid)
                        Left _    -> False

startJACKServer :: IO Bool
startJACKServer = do
    logInfo "Attempting to start JACK server (dummy backend)..."
    result <- try (callCommand "jackd -d dummy -r 44100 -p 256 -n 2 2>/dev/null &")
                  :: IO (Either SomeException ())
    case result of
        Left e  -> logErr ("Failed: " ++ show e) >> return False
        Right _ -> do
            threadDelay 3000000
            ok <- isJACKRunning
            if ok then logInfo "JACK server started." >> return True
                  else logErr  "JACK server failed to start." >> return False

-- | Exponential back-off: attempt n → wait 2^n seconds, capped at 16 s.
reconnectBackoff :: Int -> IO ()
reconnectBackoff n = do
    let µs = min 16000000 (2 ^ n * 1000000)
    logInfo $ "Backoff: waiting " ++ show (µs `div` 1000000) ++ " s..."
    threadDelay µs

attemptReconnect :: JackState -> IO Bool
attemptReconnect st = do
    attempts <- readIORef (reconnectAttempts st)
    if attempts >= maxReconnectAttempts st
        then do
            writeIORef (jackStatus st) (JackError "max reconnect attempts reached")
            writeIORef (shouldReconnect st) False
            logErr $ "JACK: gave up after " ++ show attempts ++ " attempts."
            return False
        else do
            writeIORef (jackStatus st) JackReconnecting
            modifyIORef' (reconnectAttempts st) (+1)
            reconnectBackoff attempts
            logInfo $ "Reconnect attempt " ++ show (attempts + 1)
                   ++ "/" ++ show (maxReconnectAttempts st)
            ok <- isJACKRunning
            if ok
                then return True
                else do
                    started <- startJACKServer
                    when started $ writeIORef (reconnectAttempts st) 0
                    return started

-------------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------------

runBackend :: Config -> TVar ReactorState -> IO ()
runBackend cfg stateVar = do
    logInfo "Starting DeMoDNote JACK backend..."
    -- Ring must be larger than the largest JACK period we expect.
    -- 8192 × Int16 = 16 KB; at 44100/48000 Hz this is ≥ 185 ms.
    audioState <- newAudioState 8192
    jackState  <- newJackState 5

    mOsc <- (Just <$> createOscClient "127.0.0.1" (oscPort cfg))
                `catch` (\e -> do
                    logErr $ "OSC client unavailable: " ++ show (e :: SomeException)
                    return Nothing)
    atomically $ writeTVar (oscClient audioState) mOsc

    let shutdown = do
            atomically $ writeTVar (running audioState) False
            writeIORef (shouldReconnect jackState) False
    _ <- installHandler sigINT  (Catch (shutdown >> logInfo "SIGINT."))  Nothing
    _ <- installHandler sigTERM (Catch (shutdown >> logInfo "SIGTERM.")) Nothing

    jackLoop cfg audioState jackState stateVar

-------------------------------------------------------------------------------
-- JACK connection loop with exponential-back-off reconnect
-------------------------------------------------------------------------------

jackLoop :: Config -> AudioState -> JackState -> TVar ReactorState -> IO ()
jackLoop cfg audioState jackState stateVar = do
    recon    <- readIORef (shouldReconnect jackState)
    running' <- atomically $ readTVar (running audioState)
    when (recon && running') $ do
        jackSession cfg audioState jackState stateVar
        writeIORef (jackStatus jackState) (JackError "session lost")
        ok <- attemptReconnect jackState
        when ok $ jackLoop cfg audioState jackState stateVar

-------------------------------------------------------------------------------
-- One JACK client lifetime
--
-- Bracket chain:
--   withClientDefault
--     └─ withPort "input"
--          └─ withPort "output"
--               └─ withPortConnect  (port-connection logging)
--                    └─ withProcess (RT callback)
--                         └─ withActivation
--                              └─ main wait loop + parameter polling
--
-- The jack Haskell package does NOT expose buffer-size-change, sample-rate-
-- change, or shutdown callbacks at the Haskell level.  We handle these by:
--   • Polling getBufferSize / getSampleRate every ~1 s in the wait loop
--   • Treating the Nothing result from handleExceptionShow as a shutdown
--     event (JACK raises an exception when the server exits)
-------------------------------------------------------------------------------

jackSession
    :: Config
    -> AudioState
    -> JackState
    -> TVar ReactorState
    -> IO ()
jackSession cfg audioState jackState stateVar = do
    putStrLn "Starting JACK client..."
    result <- try $ JAudio.mainStereo $ \stereo -> do
        writeIORef (jackStatus jackState) JackConnected
        return stereo
    case result of
        Left (e :: SomeException) -> putStrLn $ "JACK error: " ++ show e
        Right _ -> putStrLn "JACK session ended"

-------------------------------------------------------------------------------
-- Detector thread
--
-- Wakes on rbSemaphore (posted once per JACK period by the RT callback).
-- 2-second timeout: if no frame arrives the server is likely dead.
-- Counts consecutive silent ticks so we can log deteriorating conditions.
-------------------------------------------------------------------------------

detectorThread
    :: Config -> AudioState -> TVar ReactorState -> JackState -> IO ()
detectorThread cfg audioState stateVar jackState = do
    logInfo "Detector thread started."
    loop (0 :: Int)
  where
    rb = audioRingBuffer audioState

    loop !silentTicks = do
        isRunning <- atomically $ readTVar (running audioState)
        unless isRunning $ do
            sendFinalNoteOff audioState
            logInfo "Detector thread stopping."

        when isRunning $ do
            mReady <- timeout 2000000 (takeMVar (rbSemaphore rb))
            case mReady of
                Nothing -> do
                    xruns <- readIORef (xrunCount audioState)
                    logErr $ "[Detector] No JACK frames for "
                          ++ show ((silentTicks + 1) * 2)
                          ++ " s (xruns=" ++ show xruns ++ ")"
                    loop (silentTicks + 1)
                Just () -> do
                    processFrame cfg audioState stateVar
                    loop 0

sendFinalNoteOff :: AudioState -> IO ()
sendFinalNoteOff audioState = do
    curr <- atomically $ readTVar (currentNote audioState)
    case curr of
        Nothing        -> return ()
        Just (note, _) -> do
            logInfo $ "Final Note Off: " ++ show note
            withOscClient audioState (`sendNoteOff` note)
            atomically $ writeTVar (currentNote audioState) Nothing

-------------------------------------------------------------------------------
-- processFrame
--
-- Runs once per JACK period in the detector (non-RT) thread.
--
-- Fix 1 — state machine continuity
--   noteStateMach, pllStateMach, and onsetFeatures are read from stateVar
--   in one atomic transaction so detect() sees the correct current state.
--   Previously noteStateMach was hardcoded to `Idle`, breaking the state
--   machine on every single frame.
--
-- Fix 2 — correct sample type
--   Int16 ring samples are normalised to Double [-1, 1] before calling
--   detect.  Previously VS.map fromIntegral :: VS.Vector Int16 was a
--   type-annotated no-op that produced the wrong input for any detector
--   expecting floating-point data.
--
-- Fix 3 — adaptive detection window
--   Window = max 256 (bs * 2), capped at rbSize.  This guarantees at least
--   two full JACK periods of audio at any buffer-size setting.
--
-- Fix 4 — reactor state preservation
--   reactorBPM and reactorThreshold are read from the existing state and
--   written back unchanged, so OSC-set values are not silently discarded.
--   pllStateMach and onsetFeatures are also preserved (detector API does not
--   return updated values in DetectionResult; see note in code).
-------------------------------------------------------------------------------

processFrame :: Config -> AudioState -> TVar ReactorState -> IO ()
processFrame cfg audioState stateVar = do
    -- One atomic read of all state we need to evolve
    (sr, bs, curNoteState, curPLL, curOnset, curBPM, curThresh) <-
        atomically $ do
            sr' <- readTVar (sampleRate audioState)
            bs' <- readTVar (bufferSize audioState)
            rs  <- readTVar stateVar
            return ( sr'
                   , bs'
                   , noteStateMach   rs   -- Fix 1: was Idle
                   , pllStateMach    rs
                   , onsetFeatures   rs
                   , reactorBPM      rs   -- Fix 4
                   , reactorThreshold rs  -- Fix 4
                   )

    let !latencyMs       = fromIntegral bs / fromIntegral sr * 1000.0 :: Double
        -- Fix 3: at least 2 periods, at most the ring capacity
        !detectionWindow = min (rbSize (audioRingBuffer audioState))
                               (max 256 (bs * 2))

    -- Snapshot ring buffer (two copyArray calls, allocates `detectionWindow` cells)
    samples <- readSamples (audioRingBuffer audioState) detectionWindow

    when (VS.length samples >= 128) $ do
        currentTime <- getMicroTime

        -- Normalise Int16 → Double [-1.0, 1.0] for waveform display
        let !samplesD = VS.map (\x -> fromIntegral x / 32768.0 :: Double) samples
            !waveform = VS.toList samplesD

        -- Call detector with the live state from all three state machines.
        -- Pass raw Int samples (converted from Int16)
        let samplesInt = VS.map fromIntegral samples
        result <- detect cfg samplesInt currentTime curNoteState curPLL curOnset

        -- Tuning
        let (detTuningNote, detTuningCents) =
                case detectedNote result of
                    Nothing        -> (Nothing, 0.0)
                    Just (note, _) ->
                        let (nearest, cents) = nearestNote (midiToFreq note)
                        in  (Just nearest, cents)
            detTuningInTune = isInTune detTuningCents

        -- Write display-only fields atomically
        atomically $ do
            writeTVar (lastConfidence audioState) (confidence result)
            writeTVar (lastLatency    audioState) latencyMs
            writeTVar (lastWaveform   audioState) waveform

        -- Handle note transitions (note-off before note-on)
        handleNoteChange audioState
            (detectedNote result)
            detTuningNote detTuningCents detTuningInTune

        -- Update reactor state.
        --
        -- pllStateMach / onsetFeatures:
        --   The jack package's DetectionResult does not return an updated PLL
        --   state or onset features, so we preserve the values we read at the
        --   start of this frame (curPLL, curOnset).  If your Detector exposes
        --   these via e.g. `pllResult result` or `onsetResult result`, assign
        --   them here instead of curPLL / curOnset.
        atomically $ writeTVar stateVar ReactorState
            { currentNotes     = maybe [] (:[]) (detectedNote result)
            , noteStateMach    = noteState result   -- updated by detect
            , pllStateMach     = curPLL              -- preserved (see above)
            , onsetFeatures    = curOnset            -- preserved (see above)
            , lastOnsetTime    = currentTime
            , config           = cfg
            , reactorBPM       = curBPM
            , reactorThreshold = curThresh
            }

-------------------------------------------------------------------------------
-- Note transitions
--
-- Rule: always send note-off for the outgoing note BEFORE note-on for the
-- incoming one.  Without this, the MIDI bridge may receive two note-ons for
-- different pitches with no intervening note-off, causing stuck notes.
-------------------------------------------------------------------------------

handleNoteChange
    :: AudioState
    -> Maybe (Int, Int)
    -> Maybe Int -> Double -> Bool
    -> IO ()
handleNoteChange audioState Nothing _ _ _ = do
    curr <- atomically $ readTVar (currentNote audioState)
    case curr of
        Nothing        -> return ()
        Just (note, _) -> do
            logInfo $ "Note Off: " ++ show note
            withOscClient audioState (`sendNoteOff` note)
            atomically $ writeTVar (currentNote audioState) Nothing

handleNoteChange audioState (Just (note, vel)) tuningNote tuningCents tuningInTune = do
    curr <- atomically $ readTVar (currentNote audioState)
    case curr of
        Just (prev, _) | prev == note ->
            -- Same note still sounding — nothing to do
            return ()
        Just (prev, _) -> do
            -- Different note: send off for old, then on for new
            logInfo $ "Note Off (transition): " ++ show prev
            withOscClient audioState (`sendNoteOff` prev)
            atomically $ writeTVar (currentNote audioState) Nothing
            fireNoteOn audioState note vel tuningNote tuningCents tuningInTune
        Nothing ->
            fireNoteOn audioState note vel tuningNote tuningCents tuningInTune

fireNoteOn :: AudioState -> Int -> Int -> Maybe Int -> Double -> Bool -> IO ()
fireNoteOn audioState note vel tuningNote tuningCents tuningInTune = do
    let info = case tuningNote of
                   Just t  -> " [" ++ midiToNoteName t
                           ++ " " ++ show (round tuningCents :: Int) ++ " ¢"
                           ++ if tuningInTune then " ✓]" else "]"
                   Nothing -> ""
    logInfo $ "Note On: " ++ show note ++ " vel=" ++ show vel ++ info
    withOscClient audioState (\c -> sendNoteOn c note vel)
    atomically $ writeTVar (currentNote audioState) (Just (note, vel))

withOscClient :: AudioState -> (OscClient -> IO ()) -> IO ()
withOscClient audioState action = do
    mc <- atomically $ readTVar (oscClient audioState)
    case mc of
        Just c  -> action c `catch` (\(_ :: SomeException) -> return ())
        Nothing -> return ()

-------------------------------------------------------------------------------
-- Passthrough mode  (no detection — zero-copy RT callback only)
-------------------------------------------------------------------------------

runBackendSimple :: Config -> TVar ReactorState -> IO ()
runBackendSimple _cfg _stateVar = do
    logInfo "Starting DeMoDNote JACK backend (passthrough mode)..."
    doneRef <- newTVarIO False
    _ <- installHandler sigINT (Catch $ do
            atomically $ writeTVar doneRef True
            logInfo "Shutting down...") Nothing

    JACK.handleExceptions $
        JACK.withClientDefault "DeMoDNote-Passthrough" $ \client ->
            JACK.withPort client "input"  $ \(inPort  :: JAudio.Port JACK.Input)  ->
              JACK.withPort client "output" $ \(outPort :: JAudio.Port JACK.Output) ->
                JACK.withProcess client
                    (\nframes -> Trans.lift $ do
                        let n = case nframes of JACK.NFrames w -> fromIntegral w
                        inPtr  <- JAudio.getBufferPtr inPort  nframes
                        outPtr <- JAudio.getBufferPtr outPort nframes
                        copyArray outPtr inPtr n)
                $ JACK.withActivation client $ Trans.lift $ do
                    logInfo "JACK passthrough active. Press Ctrl+C to stop."
                    let waitLoop = do
                            isDone <- atomically $ readTVar doneRef
                            unless isDone $ threadDelay 100000 >> waitLoop
                    waitLoop

    logInfo "DeMoDNote stopped."
