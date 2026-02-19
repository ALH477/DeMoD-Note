module DeMoDNote.TUI where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.ProgressBar hiding (progressCompleteAttr, progressIncompleteAttr)
import Graphics.Vty
import Control.Concurrent (Chan, forkIO, threadDelay, readChan)
import Control.Concurrent.STM (TVar, newTVarIO, readTVar, readTVarIO, writeTVar, atomically)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import DeMoDNote.Types
import DeMoDNote.Config
import DeMoDNote.Preset (getPresetByName, Preset)
import DeMoDNote.Backend (DetectionEvent(..), JackStatus(..))

-- Safe list indexing with default value
safeIndex :: [a] -> Int -> a -> a
safeIndex [] _ def = def
safeIndex (x:_) 0 _ = x
safeIndex (_:xs) n def = safeIndex xs (n - 1) def

-- Safe tail - returns empty list for empty input  
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

-- ─────────────────────────────────────────────────────────────────────────────
-- Events
-- ─────────────────────────────────────────────────────────────────────────────

-- Unified event type for the TUI
data TUIEvent 
    = TUIStatusMsg String
    | TUIDetection DetectionEvent

-- ─────────────────────────────────────────────────────────────────────────────
-- State
-- ─────────────────────────────────────────────────────────────────────────────

data TUIState = TUIState
    { tuiConfig        :: Config
    , tuiBPM           :: Double
    , tuiTapTimes      :: [UTCTime]
    , tuiLastNote      :: Maybe (Int, Int)
    , tuiNoteHistory   :: [(Int, Int)]
    , tuiConfidence    :: Double
    , tuiLatency       :: Double
    , tuiWaveform      :: [Double]
    , tuiScaleName     :: String
    , tuiScaleIndex    :: Int
    , tuiArpeggioName  :: String
    , tuiArpeggioIndex :: Int
    , tuiRunning       :: Bool
    , tuiStatusMessage :: String
    , tuiShowHelp      :: Bool
    , tuiTuningMode    :: Bool
    , tuiTuningNote   :: Maybe Int
    , tuiTuningCents  :: Double
    , tuiTuningInTune :: Bool
    , tuiJackStatus   :: JackStatus
    }

-- Available scales / arpeggios (cycle through with s/a)
availableScales :: [String]
availableScales =
    [ "C Major", "C Minor", "C Dorian", "C Phrygian", "C Lydian"
    , "C Mixolydian", "C Locrian", "C Pentatonic Maj", "C Pentatonic Min"
    , "C Blues", "C Chromatic"
    ]

availableArpeggios :: [String]
availableArpeggios =
    [ "None", "Triad Up", "Triad Down", "Triad Up/Down"
    , "7th Up", "7th Down", "7th Up/Down", "Octave Up"
    ]

initialTUIState :: Config -> TUIState
initialTUIState cfg = TUIState
    { tuiConfig        = cfg
    , tuiBPM           = 120.0
    , tuiTapTimes      = []
    , tuiLastNote      = Nothing
    , tuiNoteHistory   = []
    , tuiConfidence    = 0.0
    , tuiLatency       = 0.0
    , tuiWaveform      = replicate 64 0.0
    , tuiScaleName     = "C Major"
    , tuiScaleIndex    = 0
    , tuiArpeggioName  = "None"
    , tuiArpeggioIndex = 0
    , tuiRunning       = True
    , tuiStatusMessage = "Ready"
    , tuiShowHelp      = False
    , tuiTuningMode    = False
    , tuiTuningNote   = Nothing
    , tuiTuningCents  = 0.0
    , tuiTuningInTune = False
    , tuiJackStatus   = JackConnected
    }

-- ─────────────────────────────────────────────────────────────────────────────
-- MIDI helpers
-- ─────────────────────────────────────────────────────────────────────────────

noteNames :: [String]
noteNames = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]

midiToName :: Int -> String
midiToName n =
    let idx = n `mod` 12
        name = safeIndex noteNames idx "C"
        octave = (n `div` 12) - 1
    in  name ++ show octave

velocityBar :: Int -> String
velocityBar v =
    let blocks = round (fromIntegral v / 127.0 * 8.0 :: Double) :: Int
        filled = replicate blocks '█'
        empty  = replicate (8 - blocks) '░'
    in filled ++ empty

-- ─────────────────────────────────────────────────────────────────────────────
-- Waveform renderer (Unicode block elements)
-- ─────────────────────────────────────────────────────────────────────────────

-- Each sample is mapped to one of 8 block chars (▁▂▃▄▅▆▇█)
-- We render a single-row spark-line style: shows amplitude envelope
sparkLine :: [Double] -> String
sparkLine samples =
    let chars = " ▁▂▃▄▅▆▇█"
        toIdx x = min 8 (max 0 (round ((abs x) * 8.0) :: Int))
    in  map (\x -> safeIndex chars (toIdx x) ' ') samples

-- Full 8-row oscilloscope block
waveformRows :: [Double] -> Int -> [String]
waveformRows samples height =
    let w      = min 64 (length samples)
        samps  = take w samples
        -- map each sample to a row index 0..height-1 (0 = bottom)
        toRow x = min (height-1) (max 0 (round ((x + 1.0) / 2.0 * fromIntegral (height-1)) :: Int))
        rows   = map toRow samps
        -- Optimize: zip with column index to avoid O(n^2) indexing
        mkLine r = [ if rowVal == r then '●' else '·' | (col, rowVal) <- zip [0..w-1] rows ]
    in  [ mkLine r | r <- [height-1, height-2..0] ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Confidence / latency bar helpers
-- ─────────────────────────────────────────────────────────────────────────────

confidenceBar :: Double -> String
confidenceBar c =
    let w      = 20
        filled = round (c * fromIntegral w) :: Int
        f      = replicate (min w filled) '█'
        e      = replicate (max 0 (w - filled)) '░'
    in  f ++ e

confidenceAttrName :: Double -> AttrName
confidenceAttrName c
    | c >= 0.75 = highConfAttr
    | c >= 0.40 = midConfAttr
    | otherwise = lowConfAttr

-- ─────────────────────────────────────────────────────────────────────────────
-- App
-- ─────────────────────────────────────────────────────────────────────────────

tuiApp :: App TUIState TUIEvent ()
tuiApp = App
    { appDraw         = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent  = handleEvent
    , appStartEvent   = return ()
    , appAttrMap      = const theMap
    }

-- ─────────────────────────────────────────────────────────────────────────────
-- Layout
-- ─────────────────────────────────────────────────────────────────────────────

drawUI :: TUIState -> [Widget ()]
drawUI state = [ui]
  where
    ui =
        withBorderStyle unicodeRounded $
        borderWithLabel (withAttr titleAttr $ str " ◈ DeMoDNote ") $
        vBox
            [ topRow state
            , hBorder
            , if tuiTuningMode state then drawTuningPanel state else drawWaveformPanel state
            , hBorder
            , midRow state
            , hBorder
            , drawNoteHistory state
            , hBorder
            , drawHelpBar state
            ]

-- ── Top row: BPM | Note | Confidence | Latency ───────────────────────────────

topRow :: TUIState -> Widget ()
topRow state =
    hBox
        [ drawJackStatusPanel state
        , vBorder
        , drawBPMPanel state
        , vBorder
        , drawNotePanel state
        , vBorder
        , drawConfidencePanel state
        , vBorder
        , drawLatencyPanel state
        ]

drawJackStatusPanel :: TUIState -> Widget ()
drawJackStatusPanel state =
    padAll 1 $
    hLimit 20 $
    vBox
        [ withAttr labelAttr (str "JACK")
        , withAttr (jackStatusAttr (tuiJackStatus state)) (str (jackStatusText (tuiJackStatus state)))
        ]

drawBPMPanel :: TUIState -> Widget ()
drawBPMPanel state =
    padAll 1 $
    hLimit 18 $
    vBox
        [ withAttr labelAttr  (str "BPM")
        , withAttr valueAttr  (str bpmStr)
        , withAttr dimAttr    (str tapIndicator)
        ]
  where
    bpmStr      = show (round (tuiBPM state) :: Int)
    tapCount    = length (tuiTapTimes state)
    tapIndicator
        | tapCount == 0 = "tap [SPACE]"
        | tapCount == 1 = "tap more…"
        | otherwise     = "taps: " ++ show tapCount

drawNotePanel :: TUIState -> Widget ()
drawNotePanel state =
    padAll 1 $
    hLimit 22 $
    vBox
        [ withAttr labelAttr (str "Last Note")
        , noteWidget
        , velWidget
        ]
  where
    noteWidget = case tuiLastNote state of
        Nothing       -> withAttr dimAttr   (str "─── ──")
        Just (n, _)   -> withAttr noteAttr  (str (midiToName n ++ "  (midi " ++ show n ++ ")"))
    velWidget = case tuiLastNote state of
        Nothing       -> withAttr dimAttr   (str "vel ░░░░░░░░")
        Just (_, v)   -> withAttr velAttr   (str ("vel " ++ velocityBar v))

drawConfidencePanel :: TUIState -> Widget ()
drawConfidencePanel state =
    padAll 1 $
    hLimit 28 $
    vBox
        [ withAttr labelAttr (str "Confidence")
        , withAttr (confidenceAttrName (tuiConfidence state))
            (str (confidenceBar (tuiConfidence state)))
        , withAttr dimAttr
            (str (show (round (tuiConfidence state * 100) :: Int) ++ "%"))
        ]

drawLatencyPanel :: TUIState -> Widget ()
drawLatencyPanel state =
    padAll 1 $
    vBox
        [ withAttr labelAttr (str "Latency")
        , withAttr valueAttr (str (showFixed1 (tuiLatency state) ++ " ms"))
        , withAttr dimAttr   (str statusIcon)
        ]
  where
    lat = tuiLatency state
    statusIcon
        | lat == 0.0 = "no data"
        | lat < 5.0  = "● excellent"
        | lat < 15.0 = "● good"
        | lat < 30.0 = "◐ moderate"
        | otherwise  = "○ high"

-- ── Waveform panel ────────────────────────────────────────────────────────────

drawWaveformPanel :: TUIState -> Widget ()
drawWaveformPanel state =
    padLeftRight 1 $
    vBox
        [ withAttr labelAttr (str ("Waveform  " ++ runningStr))
        , str ""
        , withAttr waveAttr (str (sparkLine (tuiWaveform state)))
        , str ""
        , vBox (map (withAttr waveAttr . str) (waveformRows (tuiWaveform state) 4))
        , str ""
        ]
  where
    runningStr = if tuiRunning state then "▶ running" else "⏸ paused  [p] to resume"

-- ── Tuning Panel ───────────────────────────────────────────────────────────────

drawTuningPanel :: TUIState -> Widget ()
drawTuningPanel state =
    padLeftRight 1 $
    vBox
        [ withAttr labelAttr (str "Chromatic Tuner")
        , str ""
        , noteWidget
        , str ""
        , centsWidget
        , str ""
        , indicatorWidget
        ]
  where
    noteWidget = case tuiTuningNote state of
        Nothing -> withAttr dimAttr (str "Play a note...")
        Just n -> withAttr noteAttr (str ("Note: " ++ midiToName n))
    
    cents = tuiTuningCents state
    centsWidget = 
        let centsStr = show (round cents :: Int)
            centsDisplay = if cents >= 0 then "+" ++ centsStr else centsStr
            bar = drawCentsBar cents
        in vBox
            [ withAttr (tuningAttr (tuiTuningInTune state)) (str bar)
            , withAttr valueAttr (str (centsDisplay ++ " cents"))
            ]
    
    indicatorWidget = 
        let status = if tuiTuningInTune state 
                     then "● IN TUNE" 
                     else if abs cents <= 15.0 
                          then "◐ CLOSE" 
                          else "○ OUT"
            attr = tuningAttr (tuiTuningInTune state)
        in withAttr attr (str status)

drawCentsBar :: Double -> String
drawCentsBar cents =
    let totalWidth = 25
        zeroPos = totalWidth `div` 2
        barPos = zeroPos + round (cents / 5.0)  -- 5 cents per character
        barPos' = max 0 (min (totalWidth - 1) barPos)
        left = replicate barPos' '░'
        right = replicate (totalWidth - 1 - barPos') '░'
    in left ++ "┃" ++ right

tuningAttr :: Bool -> AttrName
tuningAttr inTune
    | inTune = tuningGreenAttr
    | otherwise = tuningRedAttr

tuningGreenAttr, tuningRedAttr :: AttrName
tuningGreenAttr = attrName "tuningGreen"
tuningRedAttr = attrName "tuningRed"

jackStatusAttr :: JackStatus -> AttrName
jackStatusAttr status = case status of
    JackConnected -> jackGreenAttr
    JackDisconnected -> jackRedAttr
    JackReconnecting -> jackYellowAttr
    JackError _ -> jackRedAttr

jackGreenAttr, jackRedAttr, jackYellowAttr :: AttrName
jackGreenAttr = attrName "jackGreen"
jackRedAttr = attrName "jackRed"
jackYellowAttr = attrName "jackYellow"

jackStatusText :: JackStatus -> String
jackStatusText status = case status of
    JackConnected -> "Connected"
    JackDisconnected -> "Disconnected"
    JackReconnecting -> "Reconnecting..."
    JackError msg -> "Error: " ++ msg

-- ── Middle row: Scale | Arpeggio ─────────────────────────────────────────────

midRow :: TUIState -> Widget ()
midRow state =
    hBox
        [ drawScalePanel state
        , vBorder
        , drawArpeggioPanel state
        ]

drawScalePanel :: TUIState -> Widget ()
drawScalePanel state =
    padAll 1 $
    vBox
        [ withAttr labelAttr (str "Scale")
        , withAttr valueAttr (str (tuiScaleName state))
        , withAttr dimAttr   (str "[s] next  [S] prev")
        ]

drawArpeggioPanel :: TUIState -> Widget ()
drawArpeggioPanel state =
    padAll 1 $
    vBox
        [ withAttr labelAttr (str "Arpeggio")
        , withAttr valueAttr (str (tuiArpeggioName state))
        , withAttr dimAttr   (str "[a] next  [A] prev")
        ]

-- ── Note history ─────────────────────────────────────────────────────────────

drawNoteHistory :: TUIState -> Widget ()
drawNoteHistory state =
    padAll 1 $
    vBox
        [ withAttr labelAttr (str "Note History (newest → oldest)")
        , withAttr dimAttr   (str historyStr)
        ]
  where
    hist = tuiNoteHistory state
    historyStr
        | null hist = "─ no notes yet ─"
        | otherwise = intercalate "  " (map showEntry (take 8 hist))
    showEntry (n, v) = midiToName n ++ "/" ++ show v

-- ── Help bar ─────────────────────────────────────────────────────────────────

drawHelpBar :: TUIState -> Widget ()
drawHelpBar _ =
    withAttr helpAttr $
    padLeftRight 1 $
    str "[SPACE] tap tempo  [s/S] scale  [a/A] arpeggio  [p] pause  [r] reset  [q] quit"

-- ─────────────────────────────────────────────────────────────────────────────
-- Event handling
-- ─────────────────────────────────────────────────────────────────────────────

handleEvent :: BrickEvent () TUIEvent -> EventM () TUIState ()
handleEvent (VtyEvent e) = case e of
    EvKey (KChar 'q') [] -> halt
    EvKey KEsc         [] -> halt

    -- Tap tempo: record timestamp, compute BPM from intervals
    EvKey (KChar ' ') [] -> do
        state <- get
        now <- liftIO getCurrentTime
        let taps    = take 8 (now : tuiTapTimes state)
            newBPM  = computeBPM taps
            msg     = "Tap " ++ show (length taps)
                   ++ "  →  BPM: " ++ show (round newBPM :: Int)
        put state { tuiTapTimes = taps, tuiBPM = newBPM, tuiStatusMessage = msg }

    -- Scale cycle forward / backward
    EvKey (KChar 's') [] -> cycleScale   1
    EvKey (KChar 'S') [] -> cycleScale (-1)

    -- Arpeggio cycle forward / backward
    EvKey (KChar 'a') [] -> cycleArpeggio   1
    EvKey (KChar 'A') [] -> cycleArpeggio (-1)

    -- Pause / resume
    EvKey (KChar 'p') [] -> do
        state <- get
        let running' = not (tuiRunning state)
        put state
            { tuiRunning       = running'
            , tuiStatusMessage = if running' then "Running" else "Paused"
            }

    -- Reset tap tempo
    EvKey (KChar 'r') [] -> do
        state <- get
        put state
            { tuiTapTimes      = []
            , tuiBPM           = 120.0
            , tuiStatusMessage = "Tap tempo reset"
            }

    -- Toggle tuning mode
    EvKey (KChar 't') [] -> do
        state <- get
        let mode' = not (tuiTuningMode state)
            msg  = if mode' then "Tuning mode ON" else "Tuning mode OFF"
        put state { tuiTuningMode = mode', tuiStatusMessage = msg }

    _ -> return ()

handleEvent (AppEvent ev) = case ev of
    TUIStatusMsg msg -> do
        state <- get
        put state { tuiStatusMessage = msg }
    
    TUIDetection det -> do
        state <- get
        -- Extract data from DetectionEvent
        -- Assuming accessors deNote, deWaveform, etc. exist in Backend
        let mNote = deNote det
            hist' = case mNote of
                Nothing -> tuiNoteHistory state
                Just (n, v) -> take 8 ((n, v) : tuiNoteHistory state)
            wave' = take 64 (deWaveform det ++ repeat 0.0)
            newState = state
                { tuiLastNote    = mNote
                , tuiNoteHistory = hist'
                , tuiConfidence  = deConfidence det
                , tuiLatency     = deLatency det
                , tuiWaveform    = wave'
                }
        put newState

handleEvent _ = return ()

-- ── Helpers ──────────────────────────────────────────────────────────────────

cycleScale :: Int -> EventM () TUIState ()
cycleScale dir = do
    state <- get
    let n   = (tuiScaleIndex state + dir) `mod` length availableScales
        nm  = safeIndex availableScales n "Major"
    put state { tuiScaleIndex = n, tuiScaleName = nm, tuiStatusMessage = "Scale: " ++ nm }

cycleArpeggio :: Int -> EventM () TUIState ()
cycleArpeggio dir = do
    state <- get
    let n   = (tuiArpeggioIndex state + dir) `mod` length availableArpeggios
        nm  = safeIndex availableArpeggios n "Up"
    put state { tuiArpeggioIndex = n, tuiArpeggioName = nm, tuiStatusMessage = "Arpeggio: " ++ nm }

-- Compute BPM from a list of tap timestamps (newest first).
-- Uses average of inter-tap intervals.
computeBPM :: [UTCTime] -> Double
computeBPM []  = 120.0
computeBPM [_] = 120.0
computeBPM ts  =
    let pairs    = zip ts (safeTail ts)
        intervals = map (\(a, b) -> realToFrac (diffUTCTime a b) :: Double) pairs
        avg      = sum intervals / fromIntegral (length intervals)
    in  min 300.0 (max 20.0 (60.0 / avg))

showFixed1 :: Double -> String
showFixed1 x =
    let i  = floor x :: Int
        f  = round ((x - fromIntegral i) * 10.0) :: Int
        fStr = if f < 0 then show (abs f) else show f -- Handle negative fractional part visually if needed
    in  show i ++ "." ++ fStr

-- ─────────────────────────────────────────────────────────────────────────────
-- Attribute map
-- ─────────────────────────────────────────────────────────────────────────────

titleAttr, labelAttr, valueAttr, dimAttr :: AttrName
noteAttr, velAttr, waveAttr, helpAttr    :: AttrName
highConfAttr, midConfAttr, lowConfAttr   :: AttrName
progressCompleteAttr, progressIncompleteAttr :: AttrName


titleAttr            = attrName "title"
labelAttr            = attrName "label"
valueAttr            = attrName "value"
dimAttr              = attrName "dim"
noteAttr             = attrName "note"
velAttr              = attrName "vel"
waveAttr             = attrName "wave"
helpAttr             = attrName "help"
highConfAttr         = attrName "highConf"
midConfAttr          = attrName "midConf"
lowConfAttr          = attrName "lowConf"
progressCompleteAttr   = attrName "progressComplete"
progressIncompleteAttr = attrName "progressIncomplete"
theMap :: AttrMap
theMap = attrMap defAttr
    [ (titleAttr,    withStyle (fg cyan) bold)
    , (labelAttr,    withStyle (fg white) bold)
    , (valueAttr,    withStyle (fg brightCyan) bold)
    , (dimAttr,      fg (ISOColor 8))   -- dark gray
    , (noteAttr,     withStyle (fg brightYellow) bold)
    , (velAttr,      fg (ISOColor 6))   -- teal
    , (waveAttr,     fg (ISOColor 2))   -- green
    , (helpAttr,     fg (ISOColor 8))   -- gray
    , (highConfAttr, fg brightGreen)
    , (midConfAttr,  fg yellow)
    , (lowConfAttr,  fg red)
    , (progressCompleteAttr,   bg brightGreen)
    , (progressIncompleteAttr, bg black)
    , (tuningGreenAttr, fg brightGreen)
    , (tuningRedAttr, fg red)
    , (jackGreenAttr, fg brightGreen)
    , (jackRedAttr, fg red)
    , (jackYellowAttr, fg yellow)
    ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Entry points
-- ─────────────────────────────────────────────────────────────────────────────

runTUI :: Config -> IO ()
runTUI cfg = runTUIWithChannel cfg Nothing

runTUIWithChannel :: Config -> Maybe (Chan DetectionEvent) -> IO ()
runTUIWithChannel cfg mChan = do
    -- Create a BChan for Brick events if backend channel is provided
    mBChan <- case mChan of
        Nothing -> return Nothing
        Just chan -> do
            bc <- newBChan 100
            -- Fork a thread to translate Backend events to TUI events
            void $ forkIO $ forever $ do
                ev <- readChan chan
                writeBChan bc (TUIDetection ev)
            return (Just bc)

    let app = tuiApp
        initialState = initialTUIState cfg
    
    _ <- defaultMain app initialState
    return ()

-- Update TUI from DetectionEvent (from Backend)
-- This function is now less relevant as the channel translation happens in runTUIWithChannel,
-- but kept for reference or alternative integration patterns.
updateTUIFromDetection :: TVar TUIState -> DetectionEvent -> IO ()
updateTUIFromDetection tuiVar event = do
    case deNote event of
        Nothing -> return ()
        Just (note, vel) -> atomically $ do
            state <- readTVar tuiVar
            let hist' = take 8 ((note, vel) : tuiNoteHistory state)
                wave' = take 64 (deWaveform event ++ repeat 0.0)
            writeTVar tuiVar $ state
                { tuiLastNote    = Just (note, vel)
                , tuiNoteHistory = hist'
                , tuiConfidence = deConfidence event
                , tuiLatency    = deLatency event
                , tuiWaveform   = wave'
                }

-- Demo helpers
demoScale :: String -> IO ()
demoScale scaleName = do
    putStrLn $ "Demo scale: " ++ scaleName
    mPreset <- getPresetByName scaleName
    case mPreset of
        Nothing     -> putStrLn $ "Unknown scale: " ++ scaleName
        Just preset -> putStrLn $ show preset

demoArpeggio :: String -> String -> IO ()
demoArpeggio root pattern = do
    putStrLn $ "Demo arpeggio: " ++ root ++ " " ++ pattern
    putStrLn "Arpeggio demonstration would play here"
