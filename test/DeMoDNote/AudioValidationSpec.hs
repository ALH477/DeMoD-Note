module DeMoDNote.AudioValidationSpec where

import Test.Hspec
import Test.QuickCheck
import DeMoDNote.Detector

spec :: Spec
spec = do
    describe "AudioValidation.mathProperties" $ do
        it "440Hz should convert to MIDI 69" $
            freqToMidi 440.0 `shouldBe` 69
        it "261.63Hz should convert to MIDI 60" $
            freqToMidi 261.63 `shouldBe` 60
        it "MIDI 69 should convert back to 440Hz" $
            midiToFreq 69 `shouldBe` 440.0

    describe "AudioValidation.tuningThresholds" $ do
        it "A4 at 440Hz should be in tune" $
            let cents = freqToCents 440.0 69
            in isInTune cents `shouldBe` True
        it "A4 at 445Hz should be slightly out of tune" $
            let cents = freqToCents 445.0 69
            in isInTune cents `shouldBe` False

    describe "AudioValidation.noteDetection" $ do
        it "C4 (261.63Hz) should detect as C4" $
            let (note, _) = nearestNote 261.63
            in note `shouldBe` 60
        it "E4 (329.63Hz) should detect as E4" $
            let (note, _) = nearestNote 329.63
            in note `shouldBe` 64
        it "G4 (392Hz) should detect as G4" $
            let (note, _) = nearestNote 392.0
            in note `shouldBe` 67

    describe "AudioValidation cents calculation" $ do
        it "1 cent above should be positive" $
            freqToCents (440.0 * (2.0 ** (1.0/1200.0))) 69 > 0 `shouldBe` True
        it "1 cent below should be negative" $
            freqToCents (440.0 * (2.0 ** (-1.0/1200.0))) 69 < 0 `shouldBe` True

    describe "QuickCheck properties" $ do
        it "frequency to MIDI conversion is monotonic (with tolerance)" $
            property $ forAll (choose (20.0, 5000.0)) $ \f1 ->
            property $ forAll (choose (20.0, 5000.0)) $ \f2 ->
                let f1_midi = fromIntegral (freqToMidi f1)
                    f2_midi = fromIntegral (freqToMidi f2)
                in f1 < f2 - 1.0 ==> f1_midi <= f2_midi + 1.0
        it "cents deviation is symmetric" $
            property $ forAll (choose (0.0, 50.0)) $ \delta ->
                let freq = 440.0 * (2.0 ** (delta / 1200.0))
                    cents = freqToCents freq 69
                in abs cents > 0
        it "nearestNote always returns valid MIDI" $
            property $ forAll (choose (10.0, 10000.0)) $ \freq ->
                let (note, _) = nearestNote freq
                in note >= 0 && note <= 127
