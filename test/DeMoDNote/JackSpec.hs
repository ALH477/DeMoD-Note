module DeMoDNote.JackSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Data.IORef
import qualified Data.Vector.Storable as VS
import Foreign.C.Types (CFloat(..))
import Data.Int (Int16)
import Data.Word (Word64)

import DeMoDNote.Backend
import DeMoDNote.Config
import DeMoDNote.Types

spec :: Spec
spec = do

    -- ─────────────────────────────────────────────────────────────────────
    describe "JackState" $ do

        it "initialises: JackDisconnected, 0 attempts, reconnect=True" $ do
            st       <- newJackState 5
            status   <- readIORef (jackStatus st)
            attempts <- readIORef (reconnectAttempts st)
            recon    <- readIORef (shouldReconnect st)
            status   `shouldBe` JackDisconnected
            attempts `shouldBe` 0
            recon    `shouldBe` True

        it "stores maxReconnectAttempts correctly" $ do
            st1 <- newJackState 3
            maxReconnectAttempts st1 `shouldBe` 3
            st2 <- newJackState 10
            maxReconnectAttempts st2 `shouldBe` 10

    -- ─────────────────────────────────────────────────────────────────────
    describe "JackStatus" $ do

        it "Show instance" $ do
            show JackConnected            `shouldBe` "JackConnected"
            show JackDisconnected         `shouldBe` "JackDisconnected"
            show JackReconnecting         `shouldBe` "JackReconnecting"
            show (JackError "test error") `shouldBe` "JackError \"test error\""

        it "Eq instance" $ do
            JackConnected    `shouldBe`    JackConnected
            JackDisconnected `shouldBe`    JackDisconnected
            JackReconnecting `shouldBe`    JackReconnecting
            JackError "e"    `shouldBe`    JackError "e"
            JackConnected    `shouldNotBe` JackDisconnected
            JackError "a"    `shouldNotBe` JackError "b"

    -- ─────────────────────────────────────────────────────────────────────
    describe "DetectionEvent" $ do

        it "records all fields" $ do
            let ev = DetectionEvent
                        { deNote         = Just (60, 100)
                        , deConfidence   = 0.95
                        , deLatency      = 5.8
                        , deWaveform     = [0.0, 0.5, 1.0]
                        , deState        = Idle
                        , deTuningNote   = Just 60
                        , deTuningCents  = 5.0
                        , deTuningInTune = True
                        , deJackStatus   = JackConnected
                        }
            deNote       ev `shouldBe` Just (60, 100)
            deConfidence ev `shouldBe` 0.95
            deLatency    ev `shouldBe` 5.8
            deState      ev `shouldBe` Idle
            deJackStatus ev `shouldBe` JackConnected

    -- ─────────────────────────────────────────────────────────────────────
    describe "Ring buffer" $ do

        it "creates buffer of correct size" $ do
            rb <- newAudioRingBuffer 1024
            rbSize rb `shouldBe` 1024

        it "reads all zeros from a fresh buffer" $ do
            rb      <- newAudioRingBuffer 1024
            samples <- readSamples rb 10
            VS.length samples     `shouldBe` 10
            VS.all (== 0) samples `shouldBe` True

        it "pushSamples → readSamples round-trips a short vector" $ do
            rb <- newAudioRingBuffer 1024
            let input = VS.fromList [1, 2, 3, 4, 5] :: VS.Vector Int16
            pushSamples rb input
            output <- readSamples rb 5
            output `shouldBe` input

        it "handles wrap-around at ring boundary" $ do
            -- Ring of 8. Advance write pointer to 6 by pushing 6 zeros,
            -- then push 4 samples that must wrap across the boundary.
            rb <- newAudioRingBuffer 8
            pushSamples rb (VS.replicate 6 0)
            let wrap = VS.fromList [10, 20, 30, 40] :: VS.Vector Int16
            pushSamples rb wrap
            output <- readSamples rb 4
            output `shouldBe` wrap

        it "full-buffer write+read is correct" $ do
            rb <- newAudioRingBuffer 256
            let frame = VS.generate 256 fromIntegral :: VS.Vector Int16
            pushSamples rb frame
            output <- readSamples rb 256
            output `shouldBe` frame

        -- Fix 3: detection window is adaptive (verify ring can supply it)
        it "provides enough samples for adaptive detection window" $ do
            -- Simulate bs=512 (two periods = 1024 samples for detection)
            let bs     = 512 :: Int
                window = min 8192 (max 256 (bs * 2))  -- = 1024
            rb <- newAudioRingBuffer 8192
            -- Write enough silence to fill the window
            pushSamples rb (VS.replicate window 0)
            samples <- readSamples rb window
            VS.length samples `shouldBe` window

    -- ─────────────────────────────────────────────────────────────────────
    describe "Sample conversion" $ do

        -- cfloatToInt16 uses floor(x * 32767.5), NOT round(x * 32767.0).
        -- round(x * 32767.0) fails because:
        --   • round(-1.0 * 32767.0) = -32767  (expected -32768)
        --   • round(0.5  * 32767.0) = 16384   (banker's rounding to even)
        -- floor(x * 32767.5) gives the correct asymmetric PCM mapping.
        it "maps canonical values correctly" $ do
            cfloatToInt16 (CFloat  0.0)  `shouldBe`    0
            cfloatToInt16 (CFloat  1.0)  `shouldBe`  32767
            cfloatToInt16 (CFloat (-1.0))`shouldBe` (-32768)
            cfloatToInt16 (CFloat  0.5)  `shouldBe`  16383

        it "clamps values outside [-1, 1]" $ do
            cfloatToInt16 (CFloat  2.0)   `shouldBe`  32767
            cfloatToInt16 (CFloat (-2.0)) `shouldBe` (-32768)
            cfloatToInt16 (CFloat  100.0) `shouldBe`  32767

        -- Round-trip: max quantisation error = 1 / (2 * 32767.5) ≈ 1.5e-5
        it "round-trips within quantisation error" $ property $
            forAll (choose (-1.0, 1.0)) $ \x ->
                let i16      = cfloatToInt16 (CFloat x)
                    CFloat y = int16ToCFloat i16
                in  abs (x - y) < 1.0e-4

    -- ─────────────────────────────────────────────────────────────────────
    -- Fix 2: the normalisation Int16 → Double [-1, 1]
    -- The previous code had VS.map fromIntegral :: VS.Vector Int16 → VS.Vector Int16,
    -- which was a no-op.  The correct form is:
    --   VS.map (\x -> fromIntegral x / 32768.0 :: Double)
    describe "Int16 → Double normalisation" $ do

        it "normalises 0 → 0.0" $ do
            let v = VS.singleton (0 :: Int16)
                d = VS.map (\x -> fromIntegral x / 32768.0 :: Double) v
            VS.head d `shouldBe` 0.0

        it "normalises 32767 → ~1.0" $ do
            let v = VS.singleton (32767 :: Int16)
                d = VS.map (\x -> fromIntegral x / 32768.0 :: Double) v
            abs (VS.head d - 1.0) `shouldSatisfy` (< 1.0e-4)

        it "normalises -32768 → -1.0" $ do
            let v = VS.singleton (-32768 :: Int16)
                d = VS.map (\x -> fromIntegral x / 32768.0 :: Double) v
            VS.head d `shouldBe` (-1.0)

    -- ─────────────────────────────────────────────────────────────────────
    describe "AudioState" $ do

        it "initialises with correct defaults" $ do
            st       <- newAudioState 8192
            count    <- atomically $ readTVar (sampleCounter st)
            running' <- atomically $ readTVar (running st)
            note     <- atomically $ readTVar (currentNote st)
            xruns    <- readIORef (xrunCount st)
            sr       <- atomically $ readTVar (sampleRate st)
            bs       <- atomically $ readTVar (bufferSize st)
            count    `shouldBe` (0 :: Word64)
            running' `shouldBe` True
            note     `shouldBe` Nothing
            xruns    `shouldBe` (0 :: Word64)
            sr       `shouldBe` 44100
            bs       `shouldBe` 256

        -- Fix 4: buffer-size TVar can be updated from the wait loop
        it "bufferSize TVar accepts runtime updates" $ do
            st <- newAudioState 8192
            atomically $ writeTVar (bufferSize st) 512
            bs <- atomically $ readTVar (bufferSize st)
            bs `shouldBe` 512

        it "sampleRate TVar accepts runtime updates" $ do
            st <- newAudioState 8192
            atomically $ writeTVar (sampleRate st) 48000
            sr <- atomically $ readTVar (sampleRate st)
            sr `shouldBe` 48000

    -- ─────────────────────────────────────────────────────────────────────
    describe "JackException Show" $ do

        it "shows all constructors" $ do
            show (JackConnectionFailed "x") `shouldBe` "JackConnectionFailed \"x\""
            show (JackDisconnectedErr  "x") `shouldBe` "JackDisconnectedErr \"x\""
            show (JackReconnectFailed   3)  `shouldBe` "JackReconnectFailed 3"
            show (JackClientError      "x") `shouldBe` "JackClientError \"x\""

    -- ─────────────────────────────────────────────────────────────────────
    describe "Thread safety (STM)" $ do

        it "atomically updates sampleCounter" $ do
            st <- newAudioState 1024
            atomically $ modifyTVar' (sampleCounter st) (+1)
            atomically $ modifyTVar' (sampleCounter st) (+1)
            atomically $ modifyTVar' (sampleCounter st) (+1)
            count <- atomically $ readTVar (sampleCounter st)
            count `shouldBe` (3 :: Word64)

        it "atomically updates currentNote" $ do
            st <- newAudioState 1024
            atomically $ writeTVar (currentNote st) (Just (60, 100))
            note1 <- atomically $ readTVar (currentNote st)
            note1 `shouldBe` Just (60, 100)
            atomically $ writeTVar (currentNote st) Nothing
            note2 <- atomically $ readTVar (currentNote st)
            note2 `shouldBe` Nothing

        it "xrunCount increments atomically" $ do
            st <- newAudioState 1024
            modifyIORef' (xrunCount st) (+1)
            modifyIORef' (xrunCount st) (+1)
            n <- readIORef (xrunCount st)
            n `shouldBe` (2 :: Word64)
