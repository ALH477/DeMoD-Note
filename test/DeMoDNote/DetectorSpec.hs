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

    describe "Detector.midiToFreq" $ do
        it "MIDI 69 should be 440Hz" $
            midiToFreq 69 `shouldBe` 440.0

    describe "Detector.freqToCents" $ do
        it "exact note should be 0 cents" $
            freqToCents 440.0 69 `shouldBe` 0.0

    describe "Detector.isInTune" $ do
        it "0 cents should be in tune" $
            isInTune 0.0 `shouldBe` True
        it "5 cents should be in tune" $
            isInTune 5.0 `shouldBe` True
        it "5.1 cents should be out of tune" $
            isInTune 5.1 `shouldBe` False
        it "-5 cents should be in tune" $
            isInTune (-5.0) `shouldBe` True

    describe "Detector.isClose" $ do
        it "0 cents should be close" $
            isClose 0.0 `shouldBe` True
        it "15 cents should be close" $
            isClose 15.0 `shouldBe` True
        it "15.1 cents should not be close" $
            isClose 15.1 `shouldBe` False

    describe "Detector.midiToNoteName" $ do
        it "MIDI 60 should be C4" $
            midiToNoteName 60 `shouldBe` "C4"
        it "MIDI 69 should be A4" $
            midiToNoteName 69 `shouldBe` "A4"
        it "MIDI 61 should be C#4" $
            midiToNoteName 61 `shouldBe` "C#4"
