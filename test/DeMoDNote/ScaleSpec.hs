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

    describe "Scale.ScaleIntervals" $ do
        describe "majorScale" $ do
            it "should have 7 notes" $
                length (scaleIntervals majorScale) `shouldBe` 7
            it "should start at 0" $
                head (scaleIntervals majorScale) `shouldBe` 0

        describe "minorScale" $ do
            it "should have 7 notes" $
                length (scaleIntervals minorScale) `shouldBe` 7
            it "should start at 0" $
                head (scaleIntervals minorScale) `shouldBe` 0

        describe "pentatonicMajor" $ do
            it "should have 5 notes" $
                length (scaleIntervals pentatonicMajor) `shouldBe` 5

        describe "bluesScale" $ do
            it "should have 6 notes" $
                length (scaleIntervals bluesScale) `shouldBe` 6

    describe "Scale.getScaleByName" $ do
        it "should find major" $
            getScaleByName "major" `shouldBe` Just majorScale
        it "should find minor" $
            getScaleByName "minor" `shouldBe` Just minorScale
        it "should find pentatonic" $
            getScaleByName "pentatonic" `shouldBe` Just pentatonicMajor
        it "should return Nothing for unknown" $
            getScaleByName "nonexistent" `shouldBe` Nothing

    describe "Scale.transposeScale" $ do
        it "should transpose up by octave" $
            transposeScale majorScale 12 `shouldBe` 
                majorScale { scaleRoot = C, scaleIntervals = [12,14,16,17,19,21,23,24] }

    describe "Scale.scaleInterval" $ do
        it "should get correct interval for major scale degree 1" $
            scaleInterval majorScale 1 `shouldBe` 0
        it "should get correct interval for major scale degree 2" $
            scaleInterval majorScale 2 `shouldBe` 2
        it "should get correct interval for major scale degree 3" $
            scaleInterval majorScale 3 `shouldBe` 4

    describe "Scale.getScaleNotes" $ do
        it "should return notes for C major starting at C4" $
            take 7 (getScaleNotes majorScale C 4) `shouldBe` [60,62,64,65,67,69,71]
        it "should return notes for A minor starting at A3" $
            take 7 (getScaleNotes minorScale A 3) `shouldBe` [45,47,48,50,52,53,55]

    describe "QuickCheck properties" $ do
        prop "noteNameToMidi round-trip is stable" $
            forAll validMidi $ \n ->
                let (note, oct) = midiToNoteName n
                in noteNameToMidi note oct `shouldBe` n

        prop "midiToNoteName returns valid octave" $
            forAll validMidi $ \n ->
                let (_, oct) = midiToNoteName n
                in oct >= -1 && oct <= 9

        prop "getScaleByName returns same scale for known names" $
            forAll (elements ["major", "minor", "pentatonic", "blues"]) $ \name ->
                getScaleByName name `shouldBe` getScaleByName name

        prop "scaleInterval is within valid range" $
            forAll (scaleDegreeGen 7) $ \deg ->
                scaleInterval majorScale deg >= 0 && scaleInterval majorScale deg <= 12

validMidi :: Gen Int
validMidi = choose (0, 127)

scaleDegreeGen :: Int -> Gen Int
scaleDegreeGen n = choose (1, n)
