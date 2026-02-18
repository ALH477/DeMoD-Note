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
            it "should be less than maxBPM" $
                minBPM `shouldBe` 30.0

        describe "maxBPM" $ do
            it "should be greater than minBPM" $
                maxBPM `shouldBe` 300.0

    describe "BPM.TimeSignature" $ do
        describe "defaultTimeSignature" $ do
            it "should be 4/4" $
                tsBeats defaultTimeSignature `shouldBe` 4
            it "should have note value 4" $
                tsNoteValue defaultTimeSignature `shouldBe` 4

    describe "BPM.QuantizationGrid" $ do
        describe "getGridDivision" $ do
            it "QOff should be 0" $
                getGridDivision QOff `shouldBe` 0
            it "Q32nd should be 1/8" $
                getGridDivision Q32nd `shouldBe` (1/8)
            it "Q16th should be 1/4" $
                getGridDivision Q16th `shouldBe` 0.25
            it "Q8th should be 1/2" $
                getGridDivision Q8th `shouldBe` 0.5
            it "QQuarter should be 1" $
                getGridDivision QQuarter `shouldBe` 1.0
            it "QHalf should be 2" $
                getGridDivision QHalf `shouldBe` 2.0

        describe "QuantizationGrid Enum" $ do
            it "should have correct order" $
                toEnum 0 `shouldBe` (QOff :: QuantizationGrid)
            it "should have 6 values" $
                length [QOff, Q32nd, Q16th, Q8th, QQuarter, QHalf] `shouldBe` 6

    describe "BPM.TapState" $ do
        describe "newTapState" $ do
            it "should have empty tap times" $
                tapTimes newTapState `shouldBe` []
            it "should have zero last tap time" $
                lastTapTime newTapState `shouldBe` 0
            it "should have zero tap count" $
                tapCount newTapState `shouldBe` 0

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
                samplesToMs 96 `shouldBe` 1.0 `plusMinus` 0.01
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

    describe "BPM.mod'" $ do
        it "5 `mod'` 3 should be 2" $
            5.0 `mod'` 3.0 `shouldBe` 2.0
        it "10.5 `mod'` 3 should be 1.5" $
            10.5 `mod'` 3.0 `shouldBe` 1.5
        it "0 `mod'` 5 should be 0" $
            0.0 `mod'` 5.0 `shouldBe` 0.0
        it "negative numbers wrap correctly" $
            (-1.0) `mod'` 5.0 `shouldBe` 4.0

    describe "QuickCheck properties" $ do
        prop "msToSamples/samplesToMs round-trip is close" $
            forAll (suchThat arbitrary (>= 0)) $ \ms ->
                let samples = msToSamples ms
                    ms' = samplesToMs samples
                in ms' `shouldBe` ms `plusMinus` 0.01

        prop "beatToMs/msToBeat round-trip is close" $
            forAll (suchThat arbitrary (\b -> b > 0)) $ \bpm ->
            forAll (suchThat arbitrary (>= 0)) $ \beat ->
                let ms = beatToMs bpm beat
                    beat' = msToBeat bpm ms
                in beat' `shouldBe` beat `plusMinus` 0.01

        prop "BPM stays within valid range" $
            forAll (choose (30.0, 300.0)) $ \bpm ->
                bpm >= minBPM && bpm <= maxBPM

        prop "swing offset is always non-negative" $
            forAll (choose (0.0, 1.0)) $ \swing ->
            forAll arbitrary $ \beat ->
                let offset = if even beat then 0.0 else swing * 0.5
                in offset >= 0.0 && offset <= 0.5

plusMinus :: Double -> Double -> Double
plusMinus x d = x >= (x - d) && x <= (x + d)
