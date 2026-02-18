module DeMoDNote.TUI where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.ProgressBar hiding (progressCompleteAttr, progressIncompleteAttr)
import Graphics.Vty
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.Time.Clock
import DeMoDNote.Types
import DeMoDNote.Config
import DeMoDNote.Preset (getPresetByName, Preset)

-- TUI State
data TUIState = TUIState {
    tuiConfig :: Config,
    tuiBPM :: Double,
    tuiLastNote :: Maybe (Int, Int),  -- (Note, Velocity)
    tuiConfidence :: Double,
    tuiLatency :: Double,
    tuiWaveform :: [Double],
    tuiScaleName :: String,
    tuiArpeggioName :: String,
    tuiRunning :: Bool,
    tuiStatusMessage :: String
}

-- Initial TUI state
initialTUIState :: Config -> TUIState
initialTUIState cfg = TUIState {
    tuiConfig = cfg,
    tuiBPM = 120.0,
    tuiLastNote = Nothing,
    tuiConfidence = 0.0,
    tuiLatency = 0.0,
    tuiWaveform = replicate 64 0.0,
    tuiScaleName = "C Major",
    tuiArpeggioName = "None",
    tuiRunning = True,
    tuiStatusMessage = "Press Space to tap tempo, 'q' to quit"
}

-- Main TUI App
tuiApp :: App TUIState String ()
tuiApp = App {
    appDraw = drawUI,
    appChooseCursor = neverShowCursor,
    appHandleEvent = handleEvent,
    appStartEvent = return (),
    appAttrMap = const theMap
}

-- Draw the UI
drawUI :: TUIState -> [Widget ()]
drawUI state = [ui]
  where
    ui = withBorderStyle unicode $
         borderWithLabel (str "DeMoDNote - Deterministic Note Detector") $
         vBox [
            -- Header row
            hBox [
                drawBPM state,
                drawLastNote state,
                drawConfidence state,
                drawLatency state
            ],
            hBorder,
            -- Waveform display
            drawWaveform state,
            hBorder,
            -- Scale/Arpeggio info
            hBox [
                drawScale state,
                drawArpeggio state
            ],
            hBorder,
            -- Status message
            drawStatus state
         ]

-- Draw BPM widget
drawBPM :: TUIState -> Widget ()
drawBPM state = 
    withBorderStyle unicode $
    border $
    vBox [
        str "BPM",
        str $ show (round $ tuiBPM state),
        str "[Space to tap]"
    ]

-- Draw last detected note
drawLastNote :: TUIState -> Widget ()
drawLastNote state = 
    withBorderStyle unicode $
    border $
    vBox [
        str "Last Note",
        case tuiLastNote state of
            Nothing -> str "-"
            Just (note, vel) -> str $ show note ++ " (vel:" ++ show vel ++ ")"
    ]

-- Draw confidence meter
drawConfidence :: TUIState -> Widget ()
drawConfidence state = 
    withBorderStyle unicode $
    border $
    vBox [
        str "Confidence",
        str $ show (round $ tuiConfidence state * 100) ++ "%"
    ]

-- Draw latency display
drawLatency :: TUIState -> Widget ()
drawLatency state = 
    withBorderStyle unicode $
    border $
    vBox [
        str "Latency",
        str $ show (tuiLatency state) ++ "ms"
    ]

-- Draw waveform visualization
drawWaveform :: TUIState -> Widget ()
drawWaveform state = 
    withBorderStyle unicode $
    borderWithLabel (str "Input Waveform") $
    vBox $ map str $ waveformLines (tuiWaveform state)
  where
    waveformLines wave = 
        let height = 8
            width = length wave
            scale = map (\x -> round ((x + 1.0) / 2.0 * fromIntegral (height - 1))) wave
            rows = [[if scale !! col == row then '*' else ' ' | col <- [0..width-1]] | row <- [height-1,height-2..0]]
        in map (take 64) rows

-- Draw scale info
drawScale :: TUIState -> Widget ()
drawScale state = 
    withBorderStyle unicode $
    border $
    vBox [
        str "Scale",
        str $ tuiScaleName state,
        str "[Press 's' to change]"
    ]

-- Draw arpeggio info
drawArpeggio :: TUIState -> Widget ()
drawArpeggio state = 
    withBorderStyle unicode $
    border $
    vBox [
        str "Arpeggio",
        str $ tuiArpeggioName state,
        str "[Press 'a' to change]"
    ]

-- Draw status message
drawStatus :: TUIState -> Widget ()
drawStatus state = 
    withAttr statusAttr $
    str $ tuiStatusMessage state

-- Handle events
handleEvent :: BrickEvent () String -> EventM () TUIState ()
handleEvent (VtyEvent e) = case e of
    -- Quit
    EvKey (KChar 'q') [] -> halt
    EvKey KEsc [] -> halt
    
    -- Tap tempo
    EvKey (KChar ' ') [] -> do
        state <- get
        let newBPM = min 300.0 (tuiBPM state + 5.0)
        put state { 
            tuiBPM = newBPM,
            tuiStatusMessage = "Tapped! BPM: " ++ show (round newBPM)
        }
    
    -- Change scale
    EvKey (KChar 's') [] -> do
        state <- get
        put state { 
            tuiScaleName = "Next Scale",
            tuiStatusMessage = "Scale changed (not implemented)"
        }
    
    -- Change arpeggio
    EvKey (KChar 'a') [] -> do
        state <- get
        put state { 
            tuiArpeggioName = "Next Arpeggio",
            tuiStatusMessage = "Arpeggio changed (not implemented)"
        }
    
    -- Start/stop
    EvKey (KChar 'p') [] -> do
        state <- get
        put state { 
            tuiRunning = not (tuiRunning state),
            tuiStatusMessage = if tuiRunning state then "Paused" else "Running"
        }
    
    _ -> return ()

handleEvent (AppEvent msg) = do
    state <- get
    put state { tuiStatusMessage = msg }

handleEvent _ = return ()

-- Attribute map
theMap :: AttrMap
theMap = attrMap defAttr [
    (statusAttr, fg yellow),
    (progressCompleteAttr, bg brightGreen),
    (progressIncompleteAttr, bg black)
  ]

statusAttr :: AttrName
statusAttr = attrName "status"

progressCompleteAttr :: AttrName
progressCompleteAttr = attrName "progressComplete"

progressIncompleteAttr :: AttrName
progressIncompleteAttr = attrName "progressIncomplete"

-- Run TUI with initial state
runTUI :: Config -> IO ()
runTUI cfg = do
    let initialState = initialTUIState cfg
    _ <- defaultMain tuiApp initialState
    return ()

-- Update TUI state from reactor (would be called periodically)
updateTUIState :: TVar TUIState -> ReactorState -> IO ()
updateTUIState tuiVar reactor = do
    let current = currentNotes reactor
    case current of
        [] -> return ()
        ((note, vel):_) -> atomically $ do
            state <- readTVar tuiVar
            writeTVar tuiVar $ state {
                tuiLastNote = Just (note, vel),
                tuiConfidence = 0.95  -- Would calculate from detector
            }

-- Demo function to test scales and arpeggios
demoScale :: String -> IO ()
demoScale scaleName = do
    putStrLn $ "Demo scale: " ++ scaleName
    mPreset <- getPresetByName scaleName
    case mPreset of
        Nothing -> putStrLn $ "Unknown scale: " ++ scaleName
        Just preset -> putStrLn $ show preset

-- Demo arpeggio
demoArpeggio :: String -> String -> IO ()
demoArpeggio root pattern = do
    putStrLn $ "Demo arpeggio: " ++ root ++ " " ++ pattern
    putStrLn "Arpeggio demonstration would play here"
