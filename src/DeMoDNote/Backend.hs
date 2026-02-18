module DeMoDNote.Backend where

import Sound.JACK
import qualified Sound.JACK.Audio as JAudio
import qualified Sound.JACK.MIDI as JMIDI
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString as BS
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent (threadDelay, forkIO, yield)
import Control.Exception (bracket, try, SomeException, finally)
import Data.IORef
import Data.Bits ((.|.), (.&.))
import Data.Word (Word64)
import Foreign.C.Types (CFloat)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek, poke)
import Foreign.Marshal.Alloc (mallocBytes, free)
import System.CPUTime (getCPUTime)
import System.Posix.Signals (installHandler, sigINT, sigTERM, Handler(..))
import DeMoDNote.Types
import DeMoDNote.Config
import DeMoDNote.Detector

-- Get current time in microseconds
getMicroTime :: IO Word64
getMicroTime = do
  t <- getCPUTime
  return $ fromIntegral (t `div` 1000000)  -- Convert to microseconds

-- Convert CFloat (JACK) to Int16 representation
cfloatToInt16 :: CFloat -> Int
cfloatToInt16 x = 
  let scaled = round (realToFrac x * 32767.0) :: Int
  in max (-32768) (min 32767 scaled)

-- Simple ring buffer using immutable vectors
data RingBuffer = RingBuffer
  { rbData   :: !(IORef (VS.Vector Int))
  , rbHead   :: !(IORef Int)
  , rbSize   :: !Int
  }

newRingBuffer :: Int -> IO RingBuffer
newRingBuffer size = do
  vec <- newIORef $ VS.replicate size 0
  headRef <- newIORef 0
  return $ RingBuffer vec headRef size

-- Push single sample
pushSample :: RingBuffer -> Int -> IO ()
pushSample rb sample = do
  vec <- readIORef (rbData rb)
  headPos <- readIORef (rbHead rb)
  let newVec = vec VS.// [(headPos, sample)]
  writeIORef (rbData rb) newVec
  let newHead = (headPos + 1) `mod` rbSize rb
  writeIORef (rbHead rb) newHead

-- Get buffer of N samples
getBuffer :: RingBuffer -> Int -> IO (VS.Vector Int)
getBuffer rb n = do
  vec <- readIORef (rbData rb)
  headPos <- readIORef (rbHead rb)
  -- Read last n samples in order
  return $ VS.generate n $ \i ->
    let idx = (headPos - n + i) `mod` rbSize rb
    in vec VS.! idx

-- Encode MIDI Note On
encodeMidiNoteOn :: Int -> Int -> Int -> BS.ByteString
encodeMidiNoteOn channel note velocity = 
  BS.pack [fromIntegral (0x90 .|. (channel .&. 0x0F)), fromIntegral note, fromIntegral velocity]

-- Encode MIDI Note Off
encodeMidiNoteOff :: Int -> Int -> BS.ByteString
encodeMidiNoteOff channel note = 
  BS.pack [fromIntegral (0x80 .|. (channel .&. 0x0F)), fromIntegral note, 0]

-- Encode Pitch Bend (14-bit value, center = 8192)
encodePitchBend :: Int -> Double -> BS.ByteString
encodePitchBend channel bendSemitones = 
  let bendRange = 2.0  -- ±2 semitones
      normalized = bendSemitones / bendRange  -- -1 to 1
      value = round ((normalized + 1.0) * 8191.5)  -- 0 to 16383
      lsb = value .&. 0x7F
      msb = (value `shiftR` 7) .&. 0x7F
  in BS.pack [fromIntegral (0xE0 .|. (channel .&. 0x0F)), fromIntegral lsb, fromIntegral msb]

shiftR :: Int -> Int -> Int
shiftR = unsafeShiftR

unsafeShiftR :: Int -> Int -> Int
unsafeShiftR x n = x `div` (2^n)

-- Main JACK backend with dual-path detection
runBackend :: Config -> TVar ReactorState -> IO ()
runBackend cfg state = do
  putStrLn "Starting DeMoDNote JACK backend..."
  putStrLn $ "Sample rate: 96000 Hz"
  putStrLn $ "Buffer size: 128 samples (1.33ms)"
  putStrLn $ "Fast path: 2.66ms (200Hz+)"
  putStrLn $ "Medium path: 12ms (80-200Hz speculative)"
  putStrLn $ "Slow path: 30ms (30-80Hz validated)"
  putStrLn $ "Onset threshold: 0.67"
  
  -- Initialize ring buffer (30ms * 96kHz = 2880 samples, rounded to 4096)
  ringBuf <- newRingBuffer 4096
  
  -- Initialize detector state
  initialState <- atomically $ readTVar state
  let detectorState = emptyReactorState cfg
  
  -- Setup signal handlers
  _ <- installHandler sigINT (Catch $ putStrLn "\nReceived SIGINT, shutting down...") Nothing
  _ <- installHandler sigTERM (Catch $ putStrLn "\nReceived SIGTERM, shutting down...") Nothing
  
  -- Start detector thread
  _ <- forkIO $ detectorThread cfg ringBuf state
  
  -- Run JACK audio thread (main thread)
  runJackAudio cfg ringBuf
  
  putStrLn "DeMoDNote backend stopped."

-- JACK audio callback - just copies samples to ring buffer
runJackAudio :: Config -> RingBuffer -> IO ()
runJackAudio cfg ringBuf = 
  JAudio.mainMono $ \sample -> do
    -- Convert and push to ring buffer
    let intSample = cfloatToInt16 sample
    pushSample ringBuf intSample
    
    -- Return sample (passthrough)
    return sample

-- Detector thread - runs detection algorithms
detectorThread :: Config -> RingBuffer -> TVar ReactorState -> IO ()
detectorThread cfg ringBuf stateVar = do
  putStrLn "Detector thread started"
  detectorLoop Idle defaultPLLState defaultOnsetFeatures 0
  where
    detectorLoop nState pllState onsetState prevTime = do
      threadDelay 1333  -- 1.33ms - check every buffer
      
      currentTime <- getMicroTime
      
      -- Get current samples (128 samples = 1.33ms)
      samples <- getBuffer ringBuf 128
      
      -- Run detection
      result <- detect cfg samples currentTime nState pllState onsetState
      
      -- Handle detection result
      case detectedNote result of
        Nothing -> return ()
        Just (note, vel) -> do
          -- Check if we need pitch bend correction
          case needsBend result of
            Just (targetNote, bend) -> do
              -- Send pitch bend
              putStrLn $ "Pitch bend: " ++ show note ++ " -> " ++ show targetNote ++ " (" ++ show bend ++ " semitones)"
              -- TODO: Send MIDI pitch bend
            Nothing -> do
              -- Send note on
              putStrLn $ "Note on: " ++ show note ++ " vel=" ++ show vel ++ " conf=" ++ show (confidence result)
              -- TODO: Send MIDI note on
      
      -- Update state
      let newState = (ReactorState 
                      (case detectedNote result of Just n -> [n]; Nothing -> [])
                      (noteState result)
                      pllState
                      onsetState
                      currentTime
                      cfg)
      atomically $ writeTVar stateVar newState
      
      -- Continue loop
      detectorLoop (noteState result) pllState onsetState currentTime

-- Alternative: Full JACK implementation with explicit process callback
runBackendFull :: Config -> TVar ReactorState -> IO ()
runBackendFull cfg state = do
  putStrLn "Starting DeMoDNote JACK backend (full implementation)..."
  -- This would use withClient, newPort, setProcessCallback, etc.
  -- For now, simplified version above is used
  runBackend cfg state
