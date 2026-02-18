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
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import DeMoDNote.Types
import DeMoDNote.Config
import DeMoDNote.Preset (getPresetByName, Preset)

-- ─────────────────────────────────────────────────────────────────────────────
-- State
-- ─────────────────────────────────────────────────────────────────────────────

data TUIState = TUIState
    { tuiConfig        :: Config
    , tuiBPM           :: Double
    , tuiTapTimes      :: [UTCTime]   -- ring buffer of last 8 tap timestamps
    , tuiLastNote      :: Maybe (Int, Int)
    , tuiNoteHistory   :: [(Int, Int)] -- last 8 (note, vel) pairs, newest first
    , tuiConfidence    :: Double
    , tuiLatency       :: Double
    , tuiWaveform      :: [Double]    -- [-1.0 .. 1.0], 64 samples
    , tuiScaleName     :: String
    , tuiScaleIndex    :: Int
    , tuiArpeggioName  :: String
    , tuiArpeggioIndex :: Int
    , tuiRunning       :: Bool
    , tuiStatusMessage :: String
    , tuiShowHelp      :: Bool
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
    }

-- ─────────────────────────────────────────────────────────────────────────────
-- MIDI helpers
-- ─────────────────────────────────────────────────────────────────────────────

noteNames :: [String]
noteNames = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]

midiToName :: Int -> String
midiToName n =
    let name   = noteNames !! (n `mod` 12)
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
    in  map (\x -> chars !! toIdx x) samples

-- Full 8-row oscilloscope block
waveformRows :: [Double] -> Int -> [String]
waveformRows samples height =
    let w      = min 64 (length samples)
        samps  = take w samples
        -- map each sample to a row index 0..height-1 (0 = bottom)
        toRow x = min (height-1) (max 0 (round ((x + 1.0) / 2.0 * fromIntegral (height-1)) :: Int))
        rows   = map toRow samps
        -- for each display row (top to bottom), build the line
        mkLine r = [ if rows !! col == r then '●' else '·' | col <- [0..w-1] ]
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

tuiApp :: App TUIState String ()
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
            , drawWaveformPanel state
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
        [ drawBPMPanel state
        , vBorder
        , drawNotePanel state
        , vBorder
        , drawConfidencePanel state
        , vBorder
        , drawLatencyPanel state
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

handleEvent :: BrickEvent () String -> EventM () TUIState ()
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

    _ -> return ()

handleEvent (AppEvent msg) = do
    state <- get
    put state { tuiStatusMessage = msg }

handleEvent _ = return ()

-- ── Helpers ──────────────────────────────────────────────────────────────────

cycleScale :: Int -> EventM () TUIState ()
cycleScale dir = do
    state <- get
    let n   = (tuiScaleIndex state + dir) `mod` length availableScales
        nm  = availableScales !! n
    put state { tuiScaleIndex = n, tuiScaleName = nm, tuiStatusMessage = "Scale: " ++ nm }

cycleArpeggio :: Int -> EventM () TUIState ()
cycleArpeggio dir = do
    state <- get
    let n   = (tuiArpeggioIndex state + dir) `mod` length availableArpeggios
        nm  = availableArpeggios !! n
    put state { tuiArpeggioIndex = n, tuiArpeggioName = nm, tuiStatusMessage = "Arpeggio: " ++ nm }

-- Compute BPM from a list of tap timestamps (newest first).
-- Uses average of inter-tap intervals.
computeBPM :: [UTCTime] -> Double
computeBPM []  = 120.0
computeBPM [_] = 120.0
computeBPM ts  =
    let pairs    = zip ts (tail ts)           -- (newer, older)
        intervals = map (\(a, b) -> realToFrac (diffUTCTime a b) :: Double) pairs
        avg      = sum intervals / fromIntegral (length intervals)
    in  min 300.0 (max 20.0 (60.0 / avg))

showFixed1 :: Double -> String
showFixed1 x =
    let i  = floor x :: Int
        f  = round ((x - fromIntegral i) * 10.0) :: Int
    in  show i ++ "." ++ show f

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
    ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Entry points
-- ─────────────────────────────────────────────────────────────────────────────

runTUI :: Config -> IO ()
runTUI cfg = do
    _ <- defaultMain tuiApp (initialTUIState cfg)
    return ()

-- Update TUI state from reactor (called by your audio processing thread)
updateTUIState :: TVar TUIState -> ReactorState -> IO ()
updateTUIState tuiVar reactor = do
    let current = currentNotes reactor
    case current of
        [] -> return ()
        ((note, vel) : _) -> atomically $ do
            state <- readTVar tuiVar
            let hist' = take 8 ((note, vel) : tuiNoteHistory state)
            writeTVar tuiVar $ state
                { tuiLastNote    = Just (note, vel)
                , tuiNoteHistory = hist'
                , tuiConfidence  = 0.95   -- replace with real detector output
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
