module DeMoDNote.BackendSpec where

import Test.Hspec
import Test.QuickCheck
import DeMoDNote.Backend
import DeMoDNote.Types
import DeMoDNote.Config
import Control.Concurrent.STM (readTVarIO)
import Data.IORef (readIORef)
import Foreign.C.Types (CFloat(..))

spec :: Spec
spec = do
    describe "Backend.JackState" $ do
        it "newJackState initializes with disconnected status" $ do
            js <- newJackState 5
            status <- readIORef (jsStatus js)
            status `shouldBe` JackDisconnected
            jsMaxReconnectAttempts js `shouldBe` 5
            reconnectAttempts <- readIORef (jsReconnectAttempts js)
            reconnectAttempts `shouldBe` 0

    describe "Backend.cfloatToInt16" $ do
        it "converts 0.0 to 0" $
            cfloatToInt16 0.0 `shouldBe` 0
        
        it "converts 1.0 to 32767" $
            cfloatToInt16 1.0 `shouldBe` 32767
        
        it "converts -1.0 to -32767" $
            cfloatToInt16 (-1.0) `shouldBe` -32767
        
        it "clamps values above 1.0" $
            cfloatToInt16 2.0 `shouldBe` 32767
        
        it "clamps values below -1.0" $
            cfloatToInt16 (-2.0) `shouldBe` -32767
        
        it "converts 0.5 correctly" $
            cfloatToInt16 0.5 `shouldBe` 16384

        it "is consistent with round trip" $ property $ \f ->
            let f' = min 1.0 (max (-1.0) (f :: Float))
                i = cfloatToInt16 (realToFrac f')
            in i >= (-32767) && i <= 32767

    describe "Backend.DetectionEvent" $ do
        it "creates DetectionEvent with default values" $ do
            let ev = DetectionEvent
                    { deNote = Nothing
                    , deConfidence = 0.0
                    , deLatency = 2.66
                    , deWaveform = []
                    , deState = Idle
                    , deTuningNote = Nothing
                    , deTuningCents = 0.0
                    , deTuningInTune = True
                    , deJackStatus = JackDisconnected
                    }
            deNote ev `shouldBe` Nothing
            deState ev `shouldBe` Idle
            deJackStatus ev `shouldBe` JackDisconnected
