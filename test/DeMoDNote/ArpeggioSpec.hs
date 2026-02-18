module DeMoDNote.ArpeggioSpec where

import Test.Hspec
import Test.QuickCheck
import DeMoDNote.Arpeggio
import DeMoDNote.Scale

spec :: Spec
spec = do
    describe "Arpeggio.ChordQuality" $ do
        describe "getChordIntervals" $ do
            it "MajorTriad should be [0,4,7]" $
                getChordIntervals MajorTriad `shouldBe` [0,4,7]
            it "MinorTriad should be [0,3,7]" $
                getChordIntervals MinorTriad `shouldBe` [0,3,7]
            it "DiminishedTriad should be [0,3,6]" $
                getChordIntervals DiminishedTriad `shouldBe` [0,3,6]
            it "AugmentedTriad should be [0,4,8]" $
                getChordIntervals AugmentedTriad `shouldBe` [0,4,8]
            it "Dominant7 should be [0,4,7,10]" $
                getChordIntervals Dominant7 `shouldBe` [0,4,7,10]
            it "Major7 should be [0,4,7,11]" $
                getChordIntervals Major7 `shouldBe` [0,4,7,11]
            it "Minor7 should be [0,3,7,10]" $
                getChordIntervals Minor7 `shouldBe` [0,3,7,10]
            it "HalfDiminished7 should be [0,3,6,10]" $
                getChordIntervals HalfDiminished7 `shouldBe` [0,3,6,10]
            it "Diminished7 should be [0,3,6,9]" $
                getChordIntervals Diminished7 `shouldBe` [0,3,6,9]
            it "Suspended4 should be [0,5,7]" $
                getChordIntervals Suspended4 `shouldBe` [0,5,7]

        describe "getChordName" $ do
            it "MajorTriad should be empty" $
                getChordName MajorTriad `shouldBe` ""
            it "MinorTriad should be 'm'" $
                getChordName MinorTriad `shouldBe` "m"
            it "DiminishedTriad should be 'dim'" $
                getChordName DiminishedTriad `shouldBe` "dim"
            it "AugmentedTriad should be 'aug'" $
                getChordName AugmentedTriad `shouldBe` "aug"
            it "Dominant7 should be '7'" $
                getChordName Dominant7 `shouldBe` "7"
            it "Major7 should be 'maj7'" $
                getChordName Major7 `shouldBe` "maj7"

    describe "Arpeggio.ArpeggioPattern" $ do
        describe "showPattern" $ do
            it "Up should be 'Up'" $
                showPattern Up `shouldBe` "Up"
            it "Down should be 'Down'" $
                showPattern Down `shouldBe` "Down"
            it "UpDown should be 'Up-Down'" $
                showPattern UpDown `shouldBe` "Up-Down"
            it "DownUp should be 'Down-Up'" $
                showPattern DownUp `shouldBe` "Down-Up"
            it "Euclidean should show pattern" $
                showPattern (Euclidean 3 8) `shouldBe` "Euclidean-3-8"

    describe "Arpeggio.chordToNotes" $ do
        it "C major in octave 4" $
            chordToNotes C MajorTriad 4 `shouldBe` [60,64,67]
        it "A minor in octave 4" $
            chordToNotes A MinorTriad 4 `shouldBe` [57,60,64]
        it "G dominant7 in octave 3" $
            chordToNotes G Dominant7 3 `shouldBe` [55,59,62,65]

    describe "Arpeggio.applyPattern" $ do
        let testNotes = [0,4,7]  -- Major triad

        describe "Up" $ do
            it "should return notes in order" $
                applyPattern Up testNotes `shouldBe` [0,4,7]

        describe "Down" $ do
            it "should reverse notes" $
                applyPattern Down testNotes `shouldBe` [7,4,0]

        describe "UpDown" $ do
            it "should go up then down excluding last" $
                applyPattern UpDown testNotes `shouldBe` [0,4,7,4]

        describe "DownUp" $ do
            it "should go down then up excluding first" $
                applyPattern DownUp testNotes `shouldBe` [7,4,0,4]

    describe "Arpeggio.createArpeggio" $ do
        it "C major Up arpeggio" $
            let arp = createArpeggio C MajorTriad Up
            in arpChord arp `shouldBe` (C, MajorTriad)

        it "A minor Down arpeggio" $
            let arp = createArpeggio A MinorTriad Down
            in arpChord arp `shouldBe` (A, MinorTriad)

    describe "Arpeggio.chord constructors" $ do
        it "majorChord C" $
            let arp = majorChord C
            in arpChord arp `shouldBe` (C, MajorTriad)
        it "minorChord A" $
            let arp = minorChord A
            in arpChord arp `shouldBe` (A, MinorTriad)
        it "dominant7Chord G" $
            let arp = dominant7Chord G
            in arpChord arp `shouldBe` (G, Dominant7)

    describe "Arpeggio.euclideanPattern" $ do
        it "3 pulses in 8 steps" $
            euclideanPattern 3 8 `shouldBe` [True, False, False, True, False, False, True, False]
        it "1 pulse in 4 steps" $
            euclideanPattern 1 4 `shouldBe` [True, False, False, False]
        it "4 pulses in 4 steps" $
            euclideanPattern 4 4 `shouldBe` [True, True, True, True]

    describe "QuickCheck properties" $ do
        prop "chordToNotes has correct number of notes" $
            forAll arbitrary $ \quality ->
                let intervals = getChordIntervals quality
                in length (chordToNotes C 4 quality) `shouldBe` length intervals

        prop "applyPattern preserves all notes" $
            forAll (listOf1 (arbitrary :: Gen Int)) $ \notes ->
                length (applyPattern Up notes) `shouldBe` length notes

        prop "UpDown pattern is symmetric at extremes" $
            forAll (listOf1 (arbitrary :: Gen Int)) $ \notes ->
                let upDown = applyPattern UpDown notes
                    first = head upDown
                    last = last upDown
                in first == last || length notes == 1

        prop "getChordIntervals are all non-negative" $
            forAll arbitrary $ \quality ->
                all (>= 0) (getChordIntervals quality)

        prop "chord intervals are sorted" $
            forAll arbitrary $ \quality ->
                getChordIntervals quality == sort (getChordIntervals quality)

        prop "euclideanPattern has correct pulse count" $
            forAll (suchThat (\(p,s) -> p > 0 && s > 0 && p <= s) $ do
                p <- choose (1, 16)
                s <- choose (p, 16)
                return (p, s)) $ \(pulses, steps) ->
                let pattern = euclideanPattern pulses steps
                in length (filter id pattern) `shouldBe` pulses
