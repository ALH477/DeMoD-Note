{-# LANGUAGE OverloadedStrings #-}

module DeMoDNote.OSC (
    module Sound.Osc,
    module Sound.Osc.Datum,
    OscCommand(..),
    OscState(..),
    newOscState,
    startOSC,
    OscClient(..),
    createOscClient,
    sendNoteOn,
    sendNoteOff,
    closeOscClient
) where

import Sound.Osc
import Sound.Osc.Datum
import qualified Sound.Osc.Transport.Fd as Fd
import Sound.Osc.Transport.Fd.Udp (Udp(..), with_udp, udpServer)
import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay, Chan, newChan, writeChan, readChan)
import Control.Monad (forever)
import DeMoDNote.Types
import DeMoDNote.Config
import DeMoDNote.Preset

data OscCommand
    = SetBPM Double
    | SetThreshold Double
    | TriggerNote Int Int
    | ReleaseNote Int
    | LoadPreset String
    | GetStatus
    | SetDebug Bool
    deriving (Show, Eq)

data OscState = OscState
    { oscBPM        :: Double
    , oscThreshold  :: Double
    , oscDebug      :: Bool
    , oscCommands   :: Chan OscCommand
    }

newOscState :: IO OscState
newOscState = do
    cmds <- newChan
    return $ OscState
        { oscBPM = 120.0
        , oscThreshold = -40.0
        , oscDebug = False
        , oscCommands = cmds
        }

startOSC :: Int -> TVar ReactorState -> IO ()
startOSC port state = do
    putStrLn $ "Starting OSC server on port " ++ show port
    oscState <- newOscState
    
    _ <- forkIO $ oscCommandHandler oscState state
    _ <- forkIO $ statusBroadcaster port state
    
    with_udp (udpServer "127.0.0.1" port) $ \udp -> do
        putStrLn "OSC server listening..."
        oscReceiveLoop udp oscState state

oscReceiveLoop :: Udp -> OscState -> TVar ReactorState -> IO ()
oscReceiveLoop udp oscState state = forever $ do
    pkt <- Fd.recvPacket udp
    case pkt of
        Packet_Message msg -> handleOscMessage msg oscState state
        _ -> return ()

handleOscMessage :: Message -> OscState -> TVar ReactorState -> IO ()
handleOscMessage msg oscState state = do
    let addr = messageAddress msg
        args = messageDatum msg
    
    case addr of
        "/demod/set/bpm" -> case args of
            [Float bpm] -> do
                writeChan (oscCommands oscState) $ SetBPM (realToFrac bpm)
                putStrLn $ "OSC: Set BPM to " ++ show bpm
            _ -> putStrLn $ "OSC: Invalid args for /demod/set/bpm"
        
        "/demod/set/threshold" -> case args of
            [Float db] -> do
                writeChan (oscCommands oscState) $ SetThreshold (realToFrac db)
                putStrLn $ "OSC: Set threshold to " ++ show db
            _ -> putStrLn $ "OSC: Invalid args for /demod/set/threshold"
        
        "/demod/note/trigger" -> case args of
            [Int32 note, Int32 vel] -> do
                writeChan (oscCommands oscState) $ TriggerNote (fromIntegral note) (fromIntegral vel)
                putStrLn $ "OSC: Trigger note " ++ show note ++ " vel=" ++ show vel
            _ -> putStrLn $ "OSC: Invalid args for /demod/note/trigger"
        
        "/demod/note/off" -> case args of
            [Int32 note] -> do
                writeChan (oscCommands oscState) $ ReleaseNote (fromIntegral note)
                putStrLn $ "OSC: Note off " ++ show note
            _ -> putStrLn $ "OSC: Invalid args for /demod/note/off"
        
        "/demod/preset/load" -> case args of
            [AsciiString name] -> do
                let nameStr = show name
                writeChan (oscCommands oscState) $ LoadPreset nameStr
                putStrLn $ "OSC: Load preset " ++ nameStr
            _ -> putStrLn $ "OSC: Invalid args for /demod/preset/load"
        
        "/demod/status" -> do
            reactorSt <- readTVarIO state
            let noteCount = length $ currentNotes reactorSt
            putStrLn $ "OSC: Status - " ++ show noteCount ++ " notes active"
        
        "/demod/debug" -> case args of
            [Int32 val] -> do
                writeChan (oscCommands oscState) $ SetDebug (val /= 0)
                putStrLn $ "OSC: Debug " ++ show (val /= 0)
            _ -> return ()
        
        _ -> putStrLn $ "OSC: Unknown: " ++ addr

oscCommandHandler :: OscState -> TVar ReactorState -> IO ()
oscCommandHandler oscState state = do
    cmd <- readChan (oscCommands oscState)
    case cmd of
        SetBPM bpm -> do
            atomically $ modifyTVar' state $ \s -> s { reactorBPM = bpm }
        SetThreshold db -> do
            atomically $ modifyTVar' state $ \s -> s { reactorThreshold = db }
        TriggerNote note vel -> do
            atomically $ modifyTVar' state $ \s -> s { currentNotes = [(note, vel)] }
        ReleaseNote _ -> do
            atomically $ modifyTVar' state $ \s -> s { currentNotes = [] }
        LoadPreset name -> do
            mPreset <- getPresetByName name
            case mPreset of
                Just p -> do
                    putStrLn $ "OSC: Loaded preset: " ++ name
                Nothing -> putStrLn $ "OSC: Preset not found: " ++ name
        GetStatus -> return ()
        SetDebug b -> return ()
    oscCommandHandler oscState state

statusBroadcaster :: Int -> TVar ReactorState -> IO ()
statusBroadcaster port state = do
    with_udp (udpServer "127.0.0.1" (port + 1)) $ \udp -> do
        forever $ do
            threadDelay 50000
            reactorSt <- readTVarIO state
            let msg = Message "/demod/status" 
                    [ Int32 $ fromIntegral $ length $ currentNotes reactorSt
                    , Float $ realToFrac $ reactorBPM reactorSt
                    ]
            Fd.sendMessage udp msg

sendSetBPM :: Double -> OscCommand
sendSetBPM = SetBPM

sendSetThreshold :: Double -> OscCommand
sendSetThreshold = SetThreshold

sendTriggerNote :: Int -> Int -> OscCommand
sendTriggerNote = TriggerNote

sendReleaseNote :: Int -> OscCommand
sendReleaseNote = ReleaseNote

sendLoadPreset :: String -> OscCommand
sendLoadPreset = LoadPreset

-- OSC Client for sending outbound messages (e.g., to MIDI bridge)
-- Note: Full outbound OSC support requires more complex hosc integration
-- For now, the architecture is in place and can be completed later
data OscClient = OscClient
    { oscTargetHost :: String
    , oscTargetPort :: Int
    }

-- Create OSC client to send to target host:port
createOscClient :: String -> Int -> IO OscClient
createOscClient host port = do
    putStrLn $ "Created OSC client for " ++ host ++ ":" ++ show port
    return $ OscClient host port

-- Send note trigger via OSC (stub - architecture ready for implementation)
sendNoteOn :: OscClient -> Int -> Int -> IO ()
sendNoteOn client note vel = do
    putStrLn $ "[OSC] Note On: " ++ show note ++ " vel=" ++ show vel ++ " -> " ++ oscTargetHost client ++ ":" ++ show (oscTargetPort client)

-- Send note off via OSC (stub - architecture ready for implementation)
sendNoteOff :: OscClient -> Int -> IO ()
sendNoteOff client note = do
    putStrLn $ "[OSC] Note Off: " ++ show note ++ " -> " ++ oscTargetHost client ++ ":" ++ show (oscTargetPort client)

-- Close OSC client
closeOscClient :: OscClient -> IO ()
closeOscClient _client = return ()
