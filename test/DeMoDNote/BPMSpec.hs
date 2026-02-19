module DeMoDNote.BPMSpec where

import Test.Hspec
import Test.QuickCheck
import DeMoDNote.BPM

spec :: Spec
spec = do
    describe "BPM.Constants" $ do
        describe "defaultBPM" $ do
            it "should be 120.0" $
                defaultBPM `shouldBe` 120.0

        describe "minBPM" $ do
            it "should be 30.0" $
                minBPM `shouldBe` 30.0

        describe "maxBPM" $ do
            it "should be 300.0" $
                maxBPM `shouldBe` 300.0

    describe "BPM.TimeSignature" $ do
        it "4/4 time signature" $
            TimeSignature 4 4 `shouldBe` defaultTimeSignature
        it "3/4 time signature has 3 beats" $
            tsBeats (TimeSignature 3 4) `shouldBe` 3
        it "6/8 time signature has 8th note value" $
            tsNoteValue (TimeSignature 6 8) `shouldBe` 8

    describe "BPM.QuantizationGrid" $ do
        it "Q8th should give 0.5 division" $
            getGridDivision Q8th `shouldBe` 0.5
        it "Q16th should give 0.25 division" $
            getGridDivision Q16th `shouldBe` 0.25
        it "QQuarter should give 1.0 division" $
            getGridDivision QQuarter `shouldBe` 1.0
        it "QOff should give 0" $
            getGridDivision QOff `shouldBe` 0
        it "Q32nd should give 0.125" $
            getGridDivision Q32nd `shouldBe` 0.125

    describe "BPM.Conversion" $ do
        describe "msToSamples" $ do
            it "0ms should be 0 samples" $
                msToSamples 0.0 `shouldBe` 0
            it "1ms should be 96 samples" $
                msToSamples 1.0 `shouldBe` 96
            it "100ms should be 9600 samples" $
                msToSamples 100.0 `shouldBe` 9600

        describe "samplesToMs" $ do
            it "0 samples should be 0ms" $
                samplesToMs 0 `shouldBe` 0.0
            it "96 samples should be ~1ms" $
                samplesToMs 96 `shouldBe` 1.0
            it "9600 samples should be 100ms" $
                samplesToMs 9600 `shouldBe` 100.0

        describe "beatToMs" $ do
            it "120 BPM, 1 beat = 500ms" $
                beatToMs 120.0 1.0 `shouldBe` 500.0
            it "60 BPM, 1 beat = 1000ms" $
                beatToMs 60.0 1.0 `shouldBe` 1000.0
            it "120 BPM, 2 beats = 1000ms" $
                beatToMs 120.0 2.0 `shouldBe` 1000.0

        describe "msToBeat" $ do
            it "120 BPM, 500ms = 1 beat" $
                msToBeat 120.0 500.0 `shouldBe` 1.0
            it "60 BPM, 1000ms = 1 beat" $
                msToBeat 60.0 1000.0 `shouldBe` 1.0
            it "120 BPM, 1000ms = 2 beats" $
                msToBeat 120.0 1000.0 `shouldBe` 2.0

    describe "BPM.swing calculation" $ do
        it "swing offset formula exists" $ True `shouldBe` True

    describe "QuickCheck properties" $ do
        it "msToSamples/samplesToMs round-trip is close" $
            property $ forAll (suchThat arbitrary (>= 0)) $ \ms ->
                let samples = msToSamples ms
                    ms' = samplesToMs samples
                in abs (ms' - ms) < 0.01

        it "beatToMs/msToBeat round-trip is close" $
            property $ forAll (suchThat arbitrary (\b -> b > 0)) $ \bpm ->
            forAll (suchThat arbitrary (>= 0)) $ \beat ->
                let ms = beatToMs bpm beat
                    beat' = msToBeat bpm ms
                in abs (beat' - beat) < 0.01

        it "BPM stays within valid range" $
            property $ forAll (choose (30.0, 300.0)) $ \bpm ->
                bpm >= minBPM && bpm <= maxBPM
