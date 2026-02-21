module DeMoDNote.SoundFontSpec where

import Test.Hspec
import DeMoDNote.SoundFont

spec :: Spec
spec = do
    describe "SoundFont.paths" $ do
        it "appSoundFontDir is /etc/demod/sf" $
            appSoundFontDir `shouldBe` "/etc/demod/sf"
        
        it "systemSoundFontDir is /usr/share/soundfonts" $
            systemSoundFontDir `shouldBe` "/usr/share/soundfonts"
        
        it "defaultSoundFontPaths contains expected paths" $ do
            defaultSoundFontPaths `shouldContain` ["/etc/demod/sf"]
            defaultSoundFontPaths `shouldContain` ["/usr/share/soundfonts"]

    describe "SoundFont.validateSoundFont" $ do
        it "rejects non-existent file" $ do
            result <- validateSoundFont "/nonexistent/path/file.sf2"
            result `shouldSatisfy` isLeft
            case result of
                Left err -> err `shouldContain` "not found"
                Right _ -> expectationFailure "Should have failed"
        
        it "rejects invalid extension" $ do
            result <- validateSoundFont "/etc/passwd"
            result `shouldSatisfy` isLeft
            case result of
                Left err -> err `shouldContain` "Invalid SoundFont format"
                Right _ -> expectationFailure "Should have failed"

    describe "SoundFont.getSoundFontInfo" $ do
        it "rejects non-existent file" $ do
            result <- getSoundFontInfo "/nonexistent/path/file.sf2"
            result `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
