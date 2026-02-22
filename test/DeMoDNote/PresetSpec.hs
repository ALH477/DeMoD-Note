module DeMoDNote.PresetSpec where

import Test.Hspec
import DeMoDNote.Preset
import DeMoDNote.Config

spec :: Spec
spec = do
    describe "Preset.defaultPreset" $ do
        it "should have name 'Default'" $
            presetName defaultPreset `shouldBe` "Default"
        it "should have onset threshold 0.67" $
            presetOnsetThreshold defaultPreset `shouldBe` 0.67
        it "should have fast validation 2.66ms" $
            presetFastValidationMs defaultPreset `shouldBe` 2.66

    describe "Preset.BuiltInPreset" $ do
        it "jazzWalkingBass should exist" $
            jazzWalkingBass `shouldNotBe` defaultPreset
        it "classicalGuitar should exist" $
            classicalGuitar `shouldNotBe` defaultPreset
        it "bluesLead should exist" $
            bluesLead `shouldNotBe` defaultPreset

    describe "Preset.applyPreset" $ do
        it "should apply detection settings" $
            let newCfg = applyPreset jazzWalkingBass defaultConfig
            in onsetThresh (detection newCfg) `shouldBe` presetOnsetThreshold jazzWalkingBass
        it "should apply fast validation settings" $
            let newCfg = applyPreset classicalGuitar defaultConfig
            in fastValidationMs (detection newCfg) `shouldBe` presetFastValidationMs classicalGuitar

    describe "Preset.generatePresetTOML" $ do
        it "should include preset name in comment" $
            generatePresetTOML defaultPreset `shouldContain` "# DeMoDNote Preset: Default"
        it "should include detection section" $
            generatePresetTOML defaultPreset `shouldContain` "[detection]"
        it "should include onset threshold" $
            generatePresetTOML defaultPreset `shouldContain` "onsetThreshold"

    describe "QuickCheck properties" $ do
        it "applyPreset preserves positive port" $
            let newCfg = applyPreset jazzWalkingBass defaultConfig
            in oscPort newCfg > 0 `shouldBe` True
        it "onset threshold should be in valid range for built-in presets" $
            presetOnsetThreshold jazzWalkingBass >= 0 && presetOnsetThreshold jazzWalkingBass <= 1 `shouldBe` True
