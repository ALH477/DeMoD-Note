module DeMoDNote.DetectorSpec where

import Test.Hspec
import Test.QuickCheck
import DeMoDNote.Detector

spec :: Spec
spec = do
    describe "Detector.freqToMidi" $ do
        it "440Hz should be MIDI 69 (A4)" $
            freqToMidi 440.0 `shouldBe` 69
        it "220Hz should be MIDI 57" $
            freqToMidi 220.0 `shouldBe` 57
        it "880Hz should be MIDI 81" $
            freqToMidi 880.0 `shouldBe` 81
        it "should handle very low frequencies" $
            freqToMidi 20.0 `shouldBe` 28

    describe "Detector.midiToFreq" $ do
        it "MIDI 69 should be 440Hz" $
            midiToFreq 69 `shouldBe` 440.0
        it "MIDI 60 should be ~261.63Hz (C4)" $
            midiToFreq 60 `shouldBe` 261.6255653005986

    describe "Detector.freqToCents" $ do
        it "exact note should be 0 cents" $
            freqToCents 440.0 69 `shouldBe` 0.0
        it "5 cents sharp should be positive" $
            freqToCents (440.0 * 1.00289) 69 `shouldBe` 5.0 `plusMinus` 0.1
        it "5 cents flat should be negative" $
            freqToCents (440.0 / 1.00289) 69 `shouldBe` (-5.0) `plusMinus` 0.1

    describe "Detector.nearestNote" $ do
        it "440Hz should be A4 (69) with 0 cents" $
            nearestNote 440.0 `shouldBe` (69, 0.0)
        it "should detect note above" $
            let (note, cents) = nearestNote 466.16
            in note `shouldBe` 70
        it "should detect note below" $
            let (note, cents) = nearestNote 415.30
            in note `shouldBe` 68

    describe "Detector.isInTune" $ do
        it "0 cents should be in tune" $
            isInTune 0.0 `shouldBe` True
        it "5 cents should be in tune" $
            isInTune 5.0 `shouldBe` True
        it "5.1 cents should be out of tune" $
            isInTune 5.1 `shouldBe` False
        it "-5 cents should be in tune" $
            isInTune (-5.0) `shouldBe` True
        it "-5.1 cents should be out of tune" $
            isInTune (-5.1) `shouldBe` False

    describe "Detector.isClose" $ do
        it "0 cents should be close" $
            isClose 0.0 `shouldBe` True
        it "15 cents should be close" $
            isClose 15.0 `shouldBe` True
        it "15.1 cents should not be close" $
            isClose 15.1 `shouldBe` False

    describe "Detector.calcPitchBend" $ do
        it "exact note should need no pitch bend" $
            calcPitchBend 60 261.63 `shouldBe` 0.0 `plusMinus` 0.01
        it "should clamp to +2 semitones" $
            calcPitchBend 60 880.0 `shouldBe` 2.0
        it "should clamp to -2 semitones" $
            calcPitchBend 60 65.41 `shouldBe` (-2.0)

    describe "Detector.midiToNoteName" $ do
        it "MIDI 60 should be C4" $
            midiToNoteName 60 `shouldBe` "C4"
        it "MIDI 69 should be A4" $
            midiToNoteName 69 `shouldBe` "A4"
        it "MIDI 61 should be C#4" $
            midiToNoteName 61 `shouldBe` "C#4"

    describe "QuickCheck properties" $ do
        prop "freqToMidi is monotonic (higher freq = higher note)" $
            forAll (suchThat arbitrary (\f -> f > 20 && f < 5000)) $ \f1 ->
            forAll (suchThat arbitrary (\f -> f > f1 && f < 5000)) $ \f2 ->
                freqToMidi f1 < freqToMidi f2

        prop "nearestNote is deterministic (same input = same output)" $
            forAll (suchThat arbitrary (> 0)) $ \freq ->
                nearestNote freq == nearestNote freq

        prop "nearestNote returns valid MIDI range" $
            forAll (suchThat arbitrary (> 0)) $ \freq ->
                let (note, _) = nearestNote freq
                in note >= 0 && note <= 127

        prop "isInTune is equivalent to abs(cents) <= 5" $
            forAll arbitrary $ \cents ->
                isInTune cents == (abs cents <= 5.0)

        prop "isClose is equivalent to abs(cents) <= 15" $
            forAll arbitrary $ \cents ->
                isClose cents == (abs cents <= 15.0)

        prop "midiToNoteName has correct format" $
            forAll validMidi $ \n ->
                let name = midiToNoteName n
                in length name >= 2 && last name `elem` ['0'..'9']

validMidi :: Gen Int
validMidi = choose (0, 127)

plusMinus :: Double -> Double -> Double
plusMinus x d = x `within` (x - d, x + d)

within :: Double -> (Double, Double) -> Double
within x (lo, hi) = x >= lo && x <= hi
