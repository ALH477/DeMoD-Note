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
            it "C-1 (MIDI 0) should be 0" $
                noteNameToMidi C (-1) `shouldBe` 0
            it "G9 (MIDI 127) should be 127" $
                noteNameToMidi G 9 `shouldBe` 127

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
            it "MIDI 12 should be C0" $
                midiToNoteName 12 `shouldBe` (C, 0)

    describe "Scale.scaleInterval" $ do
        it "should get correct interval for major scale degree 1" $
            scaleInterval (majorScale C) 1 `shouldBe` 0
        it "should get correct interval for major scale degree 2" $
            scaleInterval (majorScale C) 2 `shouldBe` 2
        it "should get correct interval for major scale degree 3" $
            scaleInterval (majorScale C) 3 `shouldBe` 4

    describe "Scale.majorScale" $ do
        it "C major should have 7 notes in octave 4" $
            length (take 7 (getScaleNotes (majorScale C) 4 4)) `shouldBe` 7
        it "C major intervals should be [0,2,4,5,7,9,11]" $
            map (scaleInterval (majorScale C)) [1..7] `shouldBe` [0,2,4,5,7,9,11]
        it "G major intervals should be [0,2,4,5,7,9,11]" $
            map (scaleInterval (majorScale G)) [1..7] `shouldBe` [0,2,4,5,7,9,11]

    describe "Scale.minorScale" $ do
        it "C minor should have natural minor intervals" $
            map (scaleInterval (minorScale C)) [1..7] `shouldBe` [0,2,3,5,7,8,10]

    describe "Scale.getScaleNotes" $ do
        it "C major in octave 4 should start at C4 (60)" $
            getScaleNotes (majorScale C) 4 4 `shouldBe` [60,62,64,65,67,69,71]

    describe "Scale.transposeScale" $ do
        it "transposing C major by 2 semitones should give D major" $
            take 7 (getScaleNotes (transposeScale (majorScale C) 2) 4 4) `shouldBe` [62,64,66,67,69,71,73]

    describe "Scale.makeScale" $ do
        it "should create a scale with correct root" $
            scaleRoot (makeScale C Major) `shouldBe` C
        it "should create a scale with correct type" $
            scaleType (makeScale C Major) `shouldBe` Major
        it "should create minor scale" $
            scaleType (makeScale A MinorNatural) `shouldBe` MinorNatural

    describe "QuickCheck properties" $ do
        it "midiToNoteName round-trip is correct" $
            property $ forAll (choose (0, 127)) $ \midi ->
                let (note, oct) = midiToNoteName midi
                    midi' = noteNameToMidi note oct
                in midi' == midi
