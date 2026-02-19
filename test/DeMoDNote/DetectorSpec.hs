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
        it "261.63Hz (C4) should be MIDI 60" $
            freqToMidi 261.63 `shouldBe` 60
        it "C0 (8.18Hz) should be MIDI 0" $
            abs (freqToMidi 8.18 - 0) < 1

    describe "Detector.midiToFreq" $ do
        it "MIDI 69 should be 440Hz" $
            midiToFreq 69 `shouldBe` 440.0
        it "MIDI 60 should be ~261.63Hz" $
            abs (midiToFreq 60 - 261.63) < 0.01
        it "MIDI 0 should be ~8.18Hz" $
            abs (midiToFreq 0 - 8.18) < 0.01
        it "MIDI 127 should be ~12543Hz" $
            abs (midiToFreq 127 - 12543.0) < 1.0

    describe "Detector.freqToCents" $ do
        it "exact note should be 0 cents" $
            freqToCents 440.0 69 `shouldBe` 0.0
        it "440Hz vs A4 should be 0 cents" $
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
        it "-5.1 cents should be out of tune" $
            isInTune (-5.1) `shouldBe` False

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
        it "MIDI 62 should be D4" $
            midiToNoteName 62 `shouldBe` "D4"
        it "MIDI 48 should be C3" $
            midiToNoteName 48 `shouldBe` "C3"

    describe "Detector.nearestNote" $ do
        it "440Hz should be A4 (0 cents)" $
            nearestNote 440.0 `shouldBe` (69, 0.0)
        it "261.63Hz should be C4 (approximately 0 cents)" $
            let (note, cents) = nearestNote 261.63
            in note == 60 && abs cents < 0.1
        it "300Hz should be near D4" $
            let (note, _) = nearestNote 300.0
            in note `shouldBe` 62

    describe "QuickCheck properties" $ do
        it "midiToFreq round-trip is close" $
            property $ forAll (choose (0, 127)) $ \midi ->
                let freq = midiToFreq midi
                    midi' = freqToMidi freq
                in abs (midi' - midi) < 1
        it "isInTune is symmetric for positive/negative" $
            property $ forAll (choose (-5.0, 5.0)) $ \cents ->
                isInTune cents == isInTune (-cents)
        it "nearestNote returns valid MIDI range" $
            property $ forAll (choose (20.0, 5000.0)) $ \freq ->
                let (note, _) = nearestNote freq
                in note >= 0 && note <= 127
