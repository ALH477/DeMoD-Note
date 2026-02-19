module DeMoDNote.ArpeggioSpec where

import Test.Hspec
import Test.QuickCheck
import DeMoDNote.Arpeggio
import DeMoDNote.Scale

spec :: Spec
spec = do
    describe "Arpeggio.chordToNotes" $ do
        it "C major in octave 4" $
            chordToNotes C MajorTriad 4 `shouldBe` [60,64,67]
        it "G dominant7 in octave 3" $
            chordToNotes G Dominant7 3 `shouldBe` [55,59,62,65]
