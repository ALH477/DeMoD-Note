{-# LANGUAGE OverloadedStrings #-}

module DeMoDNote.Monitor where

import Web.Scotty
import Control.Concurrent.STM
import Data.Aeson (object, (.=))
import DeMoDNote.Types

startMonitor :: Int -> TVar ReactorState -> IO ()
startMonitor port state = scotty port $ do
  get "/status" $ do
    st <- liftIO $ readTVarIO state
    json $ object ["notes" .= currentNotes st]
