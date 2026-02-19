module DeMoDNote.ScaleSpec where

import Test.Hspec
import Test.QuickCheck
import DeMoDNote.Scale

spec :: Spec
spec = do
    describe "Scale.NoteName" $ do
        describe "noteNameToMidi" $ do
            it "C4 should be MIDI 60" $
                noteNameToMidi C 4 `shouldBe` 60
            it "A4 should be MIDI 69" $
                noteNameToMidi A 4 `shouldBe` 69
            it "C5 should be MIDI 72" $
                noteNameToMidi C 5 `shouldBe` 72

        describe "midiToNoteName" $ do
            it "MIDI 60 should be C4" $
                midiToNoteName 60 `shouldBe` (C, 4)
            it "MIDI 69 should be A4" $
                midiToNoteName 69 `shouldBe` (A, 4)
            it "MIDI 48 should be C3" $
                midiToNoteName 48 `shouldBe` (C, 3)
            it "MIDI 127 should be G9" $
                midiToNoteName 127 `shouldBe` (G, 9)
            it "MIDI 0 should be C-1" $
                midiToNoteName 0 `shouldBe` (C, -1)

    describe "Scale.scaleInterval" $ do
        it "should get correct interval for major scale degree 1" $
            scaleInterval (majorScale C) 1 `shouldBe` 0
        it "should get correct interval for major scale degree 2" $
            scaleInterval (majorScale C) 2 `shouldBe` 2
        it "should get correct interval for major scale degree 3" $
            scaleInterval (majorScale C) 3 `shouldBe` 4
