module DeMoDNote.ArpeggioSpec where

import Test.Hspec
import DeMoDNote.Arpeggio
import DeMoDNote.Scale (NoteName(..))

spec :: Spec
spec = do
    describe "Arpeggio.chordToNotes" $ do
        it "C major in octave 4" $
            chordToNotes C MajorTriad 4 `shouldBe` [60,64,67]
        it "G dominant7 in octave 3" $
            chordToNotes G Dominant7 3 `shouldBe` [55,59,62,65]
        it "A minor in octave 4" $
            chordToNotes A MinorTriad 4 `shouldBe` [69,72,76]
        it "C diminished in octave 4" $
            chordToNotes C DiminishedTriad 4 `shouldBe` [60,63,66]
        it "C augmented in octave 4" $
            chordToNotes C AugmentedTriad 4 `shouldBe` [60,64,68]

    describe "Arpeggio.chord constructors" $ do
        it "majorChord should create correct arpeggio" $
            arpChord (majorChord C) `shouldBe` (C, MajorTriad)
        it "minorChord should create correct arpeggio" $
            arpChord (minorChord A) `shouldBe` (A, MinorTriad)
        it "dominant7Chord should create correct arpeggio" $
            arpChord (dominant7Chord G) `shouldBe` (G, Dominant7)
        it "minor7Chord should create correct arpeggio" $
            arpChord (minor7Chord D) `shouldBe` (D, Minor7)
        it "major7Chord should create correct arpeggio" $
            arpChord (major7Chord F) `shouldBe` (F, Major7)

    describe "Arpeggio.pattern" $ do
        it "upPattern should set pattern to Up" $
            arpPattern (upPattern 1 (majorChord C)) `shouldBe` Up
        it "downPattern should set pattern to Down" $
            arpPattern (downPattern 1 (majorChord C)) `shouldBe` Down
        it "upDownPattern should set pattern to UpDown" $
            arpPattern (upDownPattern 1 (majorChord C)) `shouldBe` UpDown

    describe "Arpeggio.getArpeggioNotes" $ do
        it "should generate notes for Up pattern" $
            take 4 (getArpeggioNotes (createArpeggio C MajorTriad Up) 4) `shouldBe` [60,64,67,72]
        it "should generate notes for Down pattern" $
            take 4 (getArpeggioNotes (createArpeggio C MajorTriad Down) 4) `shouldBe` [79,76,72,67]

    describe "Arpeggio.applyPattern" $ do
        let notes = [60,64,67,72]
        it "Up should return notes in order" $
            applyPattern Up notes `shouldBe` notes
        it "Down should reverse notes" $
            applyPattern Down notes `shouldBe` reverse notes
        it "UpDown should go up then down (without duplicate)" $
            applyPattern UpDown notes `shouldBe` notes ++ reverse (init notes)

    describe "Arpeggio.allArpeggioPatterns" $ do
        it "should contain common patterns" $
            "up" `shouldBe` case allArpeggioPatterns of (x:_) -> x; [] -> ""
        it "should contain down pattern" $
            "down" `shouldBe` allArpeggioPatterns !! 1
