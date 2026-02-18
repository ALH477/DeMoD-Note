{-# LANGUAGE OverloadedStrings #-}

module DeMoDNote.OSC where

import Sound.Osc
import qualified Sound.Osc.Transport.Fd.Udp as Udp
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Monad
import DeMoDNote.Types
import DeMoDNote.Config

startOSC :: Int -> TVar ReactorState -> IO ()
startOSC port state = do
  let udp = Udp.udpServer "127.0.0.1" port
  -- For now, stub implementation - proper implementation needs understanding the exact hosc API
  putStrLn $ "OSC server would start on port " ++ show port
  forever $ threadDelay 1000000
