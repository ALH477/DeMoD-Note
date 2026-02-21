{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DeMoDNote.Error (
    DeMoDError(..),
    renderDeMoDError,
    tryDeMoD,
    catchDeMoD,
    handleDeMoD,
    throwDeMoD
) where

import Control.Exception (Exception, SomeException, try, catch, handle, throw)
import Data.Typeable (Typeable)

data DeMoDError
    = JACKError String
    | OSCError String
    | AudioError String
    | ConfigError String
    | SoundFontError String
    | PresetError String
    | DetectorError String
    | TUIError String
    | MonitorError String
    deriving (Typeable, Show)

instance Exception DeMoDError

renderDeMoDError :: DeMoDError -> String
renderDeMoDError err = case err of
    JACKError msg -> "JACK error: " ++ msg
    OSCError msg -> "OSC error: " ++ msg
    AudioError msg -> "Audio error: " ++ msg
    ConfigError msg -> "Configuration error: " ++ msg
    SoundFontError msg -> "SoundFont error: " ++ msg
    PresetError msg -> "Preset error: " ++ msg
    DetectorError msg -> "Detector error: " ++ msg
    TUIError msg -> "TUI error: " ++ msg
    MonitorError msg -> "Monitor error: " ++ msg

tryDeMoD :: IO a -> IO (Either DeMoDError a)
tryDeMoD action = do
    result <- try action
    return $ case result of
        Left (e :: DeMoDError) -> Left e
        Right a -> Right a

catchDeMoD :: IO a -> (DeMoDError -> IO a) -> IO a
catchDeMoD action handler = do
    result <- try action
    case result of
        Left (e :: DeMoDError) -> handler e
        Right a -> return a

handleDeMoD :: (DeMoDError -> IO a) -> IO a -> IO a
handleDeMoD = flip catchDeMoD

throwDeMoD :: DeMoDError -> IO a
throwDeMoD = throw
