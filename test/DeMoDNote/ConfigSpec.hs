module DeMoDNote.ConfigSpec where

import Test.Hspec
import Test.QuickCheck
import DeMoDNote.Config

spec :: Spec
spec = do
    describe "Config.defaultConfig" $ do
        it "should have default OSC port of 57120" $
            oscPort defaultConfig `shouldBe` 57120
        it "should have default monitor port of 8080" $
            monitorPort defaultConfig `shouldBe` 8080
        it "should have Jack backend" $
            backend defaultConfig `shouldBe` Jack

    describe "DetectionConfig" $ do
        it "default algorithm should be hybrid" $
            algorithm (detection defaultConfig) `shouldBe` "Hybrid"
        it "default window size should be 2048" $
            windowSize (detection defaultConfig) `shouldBe` 2048
        it "default hop size should be 128" $
            hopSize (detection defaultConfig) `shouldBe` 128
        it "default max polyphony should be 1" $
            maxPolyphony (detection defaultConfig) `shouldBe` 1
        it "default onset threshold should be negative (dB)" $
            onsetThresh (detection defaultConfig) < 0 `shouldBe` True

    describe "TimingConfig" $ do
        it "default sample rate should be 96000" $
            sampleRate (timing defaultConfig) `shouldBe` 96000
        it "default buffer size should be 128" $
            bufferSize (timing defaultConfig) `shouldBe` 128

    describe "RTConfig" $ do
        it "default singleCore should be True" $
            singleCore (rt defaultConfig) `shouldBe` True

    describe "OnsetConfig" $ do
        it "default spectral weight should be positive" $
            spectralWeight (onset defaultConfig) > 0 `shouldBe` True
        it "default threshold should be positive" $
            threshold (onset defaultConfig) > 0 `shouldBe` True

    describe "QuickCheck properties" $ do
        it "onset threshold should be in valid range" $
            property $ forAll (choose (0.0, 1.0)) $ \thresh ->
                let cfg = defaultConfig { detection = (detection defaultConfig) { onsetThresh = thresh } }
                in onsetThresh (detection cfg) >= 0 && onsetThresh (detection cfg) <= 1
        it "window size should be power of 2" $
            property $ forAll (suchThat arbitrary (> 0)) $ \n ->
                let cfg = defaultConfig { detection = (detection defaultConfig) { windowSize = n } }
                in windowSize (detection cfg) > 0
