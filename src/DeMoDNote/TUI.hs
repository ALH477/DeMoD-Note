module DeMoDNote.TUI where

import Brick hiding (clamp)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (defaultConfig)
import Control.Concurrent (Chan, forkIO, readChan, threadDelay)
import Control.Monad (forever, void, when)
import qualified System.Process as System.Process
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (modify')
import Data.List (intercalate, intersperse)
import Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime)
import DeMoDNote.Types
import DeMoDNote.Config hiding (defaultConfig)
import DeMoDNote.Backend (DetectionEvent(..), JackStatus(..))

-- ─────────────────────────────────────────────────────────────────────────────
-- Utilities
-- ─────────────────────────────────────────────────────────────────────────────

safeIndex :: [a] -> Int -> a -> a
safeIndex []     _ d = d
safeIndex (x:_)  0 _ = x
safeIndex (_:xs) n d = safeIndex xs (n - 1) d

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi

-- "BPM" → "B P M"  (retro digital-display aesthetic)
spaced :: String -> String
spaced = intersperse ' '

-- Fixed one-decimal formatter: correct for negatives and rounding carry.
showFixed1 :: Double -> String
showFixed1 x
    | x < 0    = "-" ++ showFixed1 (abs x)
    | otherwise =
        let i        = floor x :: Int
            f        = round ((x - fromIntegral i) * 10.0) :: Int
            (i', f') = if f >= 10 then (i + 1, 0) else (i, f)
        in  show i' ++ "." ++ show f'

-- ─────────────────────────────────────────────────────────────────────────────
-- Events
-- ─────────────────────────────────────────────────────────────────────────────

data TUIEvent
    = TUIStatusMsg  String
    | TUIDetection  DetectionEvent
    | TUITick           -- fired ~4 Hz by the animation thread

-- ─────────────────────────────────────────────────────────────────────────────
-- Color palette system
-- ─────────────────────────────────────────────────────────────────────────────

data ColorPalette
    = Synthwave   -- ① neon cyan / magenta / green (default)
    | Sunset      -- ② warm red / orange / yellow
    | Matrix      -- ③ monochrome green phosphor
    | Ice         -- ④ cool blue / cyan / white
    | Amber       -- ⑤ classic amber terminal
    deriving (Eq, Enum, Bounded)

allPalettes :: [ColorPalette]
allPalettes = [minBound .. maxBound]

paletteName :: ColorPalette -> String
paletteName Synthwave = "Synthwave"
paletteName Sunset    = "Sunset"
paletteName Matrix    = "Matrix"
paletteName Ice       = "Ice"
paletteName Amber     = "Amber"

paletteKey :: ColorPalette -> Char
paletteKey Synthwave = '1'
paletteKey Sunset    = '2'
paletteKey Matrix    = '3'
paletteKey Ice       = '4'
paletteKey Amber     = '5'

paletteFromChar :: Char -> Maybe ColorPalette
paletteFromChar '1' = Just Synthwave
paletteFromChar '2' = Just Sunset
paletteFromChar '3' = Just Matrix
paletteFromChar '4' = Just Ice
paletteFromChar '5' = Just Amber
paletteFromChar _   = Nothing

-- Dynamic attr map: called on every render with the current state.
paletteMap :: ColorPalette -> AttrMap
paletteMap p = attrMap defAttr $
    [ (helpAttr,             fg (ISOColor 8))
    , (progressIncompleteAttr, bg (ISOColor 8))
    ] ++ paletteAttrs p

paletteAttrs :: ColorPalette -> [(AttrName, Attr)]
paletteAttrs Synthwave =
    [ (titleAttr,       withStyle (fg brightCyan)    bold)
    , (valueAttr,       withStyle (fg brightCyan)    bold)
    , (dimAttr,         fg (ISOColor 8))
    , (bpmAttr,         withStyle (fg brightCyan)    bold)
    , (velAttr,         fg cyan)
    , (waveAttr,        fg brightGreen)
    , (highConfAttr,    fg brightGreen)
    , (midConfAttr,     fg yellow)
    , (lowConfAttr,     fg red)
    , (heroNoteAttr,    withStyle (fg brightMagenta) bold)
    , (heroOctAttr,     fg (ISOColor 8))
    , (tuningGoodAttr,  withStyle (fg brightGreen)   bold)
    , (tuningBadAttr,   withStyle (fg red)           bold)
    , (jackGoodAttr,    fg brightGreen)
    , (jackBadAttr,     fg red)
    , (jackWarnAttr,    fg yellow)
    , (accentAttr,      fg cyan)
    , (statusAttr,      fg (ISOColor 7))
    , (splashTitleAttr, withStyle (fg brightMagenta) bold)
    , (splashSubAttr,   fg cyan)
    , (splashDivAttr,   fg (ISOColor 8))
    , (splashBodyAttr,  fg (ISOColor 7))
    , (progressCompleteAttr, bg brightGreen)
    ]
paletteAttrs Sunset =
    [ (titleAttr,       withStyle (fg red)           bold)
    , (valueAttr,       withStyle (fg brightYellow)  bold)
    , (dimAttr,         fg (ISOColor 8))
    , (bpmAttr,         withStyle (fg brightYellow)  bold)
    , (velAttr,         fg yellow)
    , (waveAttr,        fg yellow)
    , (highConfAttr,    fg yellow)
    , (midConfAttr,     fg red)
    , (lowConfAttr,     fg magenta)
    , (heroNoteAttr,    withStyle (fg brightYellow)  bold)
    , (heroOctAttr,     fg (ISOColor 8))
    , (tuningGoodAttr,  withStyle (fg yellow)        bold)
    , (tuningBadAttr,   withStyle (fg magenta)       bold)
    , (jackGoodAttr,    fg yellow)
    , (jackBadAttr,     fg magenta)
    , (jackWarnAttr,    fg red)
    , (accentAttr,      fg red)
    , (statusAttr,      fg (ISOColor 7))
    , (splashTitleAttr, withStyle (fg brightYellow)  bold)
    , (splashSubAttr,   fg red)
    , (splashDivAttr,   fg (ISOColor 8))
    , (splashBodyAttr,  fg (ISOColor 7))
    , (progressCompleteAttr, bg yellow)
    ]
paletteAttrs Matrix =
    [ (titleAttr,       withStyle (fg brightGreen)   bold)
    , (valueAttr,       withStyle (fg brightGreen)   bold)
    , (dimAttr,         fg green)
    , (bpmAttr,         withStyle (fg brightGreen)   bold)
    , (velAttr,         fg green)
    , (waveAttr,        fg brightGreen)
    , (highConfAttr,    fg brightGreen)
    , (midConfAttr,     fg green)
    , (lowConfAttr,     fg (ISOColor 2))
    , (heroNoteAttr,    withStyle (fg brightGreen)   bold)
    , (heroOctAttr,     fg green)
    , (tuningGoodAttr,  withStyle (fg brightGreen)   bold)
    , (tuningBadAttr,   withStyle (fg green)         bold)
    , (jackGoodAttr,    fg brightGreen)
    , (jackBadAttr,     fg (ISOColor 2))
    , (jackWarnAttr,    fg green)
    , (accentAttr,      fg green)
    , (statusAttr,      fg green)
    , (splashTitleAttr, withStyle (fg brightGreen)   bold)
    , (splashSubAttr,   fg green)
    , (splashDivAttr,   fg (ISOColor 2))
    , (splashBodyAttr,  fg green)
    , (progressCompleteAttr, bg brightGreen)
    ]
paletteAttrs Ice =
    [ (titleAttr,       withStyle (fg brightCyan)    bold)
    , (valueAttr,       withStyle (fg brightCyan)    bold)
    , (dimAttr,         fg (ISOColor 8))
    , (bpmAttr,         withStyle (fg brightCyan)    bold)
    , (velAttr,         fg cyan)
    , (waveAttr,        fg cyan)
    , (highConfAttr,    fg brightCyan)
    , (midConfAttr,     fg blue)
    , (lowConfAttr,     fg (ISOColor 4))
    , (heroNoteAttr,    withStyle (fg brightWhite)   bold)
    , (heroOctAttr,     fg cyan)
    , (tuningGoodAttr,  withStyle (fg brightCyan)    bold)
    , (tuningBadAttr,   withStyle (fg blue)          bold)
    , (jackGoodAttr,    fg brightCyan)
    , (jackBadAttr,     fg blue)
    , (jackWarnAttr,    fg cyan)
    , (accentAttr,      fg brightBlue)
    , (statusAttr,      fg (ISOColor 7))
    , (splashTitleAttr, withStyle (fg brightWhite)   bold)
    , (splashSubAttr,   fg brightCyan)
    , (splashDivAttr,   fg (ISOColor 8))
    , (splashBodyAttr,  fg (ISOColor 7))
    , (progressCompleteAttr, bg brightCyan)
    ]
paletteAttrs Amber =
    [ (titleAttr,       withStyle (fg yellow)        bold)
    , (valueAttr,       withStyle (fg brightYellow)  bold)
    , (dimAttr,         fg (ISOColor 8))
    , (bpmAttr,         withStyle (fg brightYellow)  bold)
    , (velAttr,         fg yellow)
    , (waveAttr,        fg yellow)
    , (highConfAttr,    fg brightYellow)
    , (midConfAttr,     fg yellow)
    , (lowConfAttr,     fg (ISOColor 3))
    , (heroNoteAttr,    withStyle (fg brightYellow)  bold)
    , (heroOctAttr,     fg yellow)
    , (tuningGoodAttr,  withStyle (fg brightYellow)  bold)
    , (tuningBadAttr,   withStyle (fg (ISOColor 3))  bold)
    , (jackGoodAttr,    fg brightYellow)
    , (jackBadAttr,     fg (ISOColor 3))
    , (jackWarnAttr,    fg yellow)
    , (accentAttr,      fg yellow)
    , (statusAttr,      fg (ISOColor 7))
    , (splashTitleAttr, withStyle (fg brightYellow)  bold)
    , (splashSubAttr,   fg yellow)
    , (splashDivAttr,   fg (ISOColor 8))
    , (splashBodyAttr,  fg (ISOColor 7))
    , (progressCompleteAttr, bg brightYellow)
    ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Tips & JACK guide (hardcoded; one tip selected pseudo-randomly at startup)
-- ─────────────────────────────────────────────────────────────────────────────

tips :: [String]
tips =
    [ "Sustain each note for 2+ beats to maximise detection confidence."
    , "Set input gain so peaks sit around -12 dBFS: not too loud, not too quiet."
    , "Open the chromatic tuner with [t] before a session to check intonation."
    , "Four or more tap-tempo taps gives the most accurate BPM estimate."
    , "High confidence (● strong) means the pitch algorithm has converged."
    , "JACK latency under 5 ms is excellent for real-time harmonic analysis."
    , "Pentatonic scales minimise 'wrong note' detections during improvisation."
    , "The oscilloscope shows raw input — watch for clipping at the top rail."
    , "Arpeggio patterns are quantised to the active scale for musical output."
    , "Try the Blues scale in C for an authentic twelve-bar feel."
    , "Connect your instrument directly before JACK routing for lowest latency."
    , "Hold a drone and cycle scales with [s] to hear how modes colour pitch."
    , "The waveform spark-line above the dot-plot shows the amplitude envelope."
    , "Press [r] to clear tap tempo and re-lock BPM to a new section."
    ]

jackGuide :: [String]
jackGuide =
    [ "① Start jackd or QjackCtl before launching DeMoDNote."
    , "② Select your audio interface as the JACK driver."
    , "③ Set buffer size to 64–256 samples for low latency."
    , "④ Connect your instrument input → DeMoDNote capture port."
    , "⑤ Press ENTER when JACK is running and routing is confirmed."
    ]

-- ─────────────────────────────────────────────────────────────────────────────
-- TUI mode
-- ─────────────────────────────────────────────────────────────────────────────

data TUIMode
    = StartMode      -- animated splash / options screen
    | MainMode       -- main monitoring interface
    | SoundFontMode  -- soundfont browser with donation prompt
    deriving (Eq)

-- ─────────────────────────────────────────────────────────────────────────────
-- State
-- ─────────────────────────────────────────────────────────────────────────────

data TUIState = TUIState
    { tuiConfig        :: Config
    -- ── mode & animation ───────────────────────────────────────────────────
    , tuiMode          :: TUIMode
    , tuiTick          :: Int          -- incremented ~4 Hz for animations
    , tuiPalette       :: ColorPalette
    , tuiTip           :: String       -- selected at startup
    -- ── audio / detection ─────────────────────────────────────────────────
    , tuiBPM           :: Double
    , tuiTapTimes      :: [UTCTime]
    , tuiLastNote      :: Maybe (Int, Int)  -- (midiNote, velocity 0–127)
    , tuiNoteHistory   :: [(Int, Int)]
    , tuiConfidence    :: Double            -- 0.0 – 1.0
    , tuiLatency       :: Double            -- milliseconds
    , tuiWaveform      :: [Double]          -- 64 samples, -1.0 – 1.0
    -- ── presets ───────────────────────────────────────────────────────────
    , tuiScaleIndex    :: Int
    , tuiArpeggioIndex :: Int
    -- ── misc ──────────────────────────────────────────────────────────────
    , tuiRunning       :: Bool
    , tuiStatusMessage :: String
    , tuiTuningMode    :: Bool
    , tuiTuningNote    :: Maybe Int
    , tuiTuningCents   :: Double            -- -50.0 – 50.0
    , tuiTuningInTune  :: Bool
    , tuiJackStatus    :: JackStatus
    }

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

currentScale :: TUIState -> String
currentScale s = safeIndex availableScales (tuiScaleIndex s) "Unknown"

currentArpeggio :: TUIState -> String
currentArpeggio s = safeIndex availableArpeggios (tuiArpeggioIndex s) "Unknown"

initialTUIState :: Config -> String -> TUIState
initialTUIState cfg tip = TUIState
    { tuiConfig        = cfg
    , tuiMode          = StartMode
    , tuiTick          = 0
    , tuiPalette       = Synthwave
    , tuiTip           = tip
    , tuiBPM           = 120.0
    , tuiTapTimes      = []
    , tuiLastNote      = Nothing
    , tuiNoteHistory   = []
    , tuiConfidence    = 0.0
    , tuiLatency       = 0.0
    , tuiWaveform      = replicate 64 0.0
    , tuiScaleIndex    = 0
    , tuiArpeggioIndex = 0
    , tuiRunning       = True
    , tuiStatusMessage = "READY  ─  PRESS ENTER TO START"
    , tuiTuningMode    = False
    , tuiTuningNote    = Nothing
    , tuiTuningCents   = 0.0
    , tuiTuningInTune  = False
    , tuiJackStatus    = JackConnected  -- Assume connected, backend handles JACK
    }

-- ─────────────────────────────────────────────────────────────────────────────
-- MIDI helpers
-- ─────────────────────────────────────────────────────────────────────────────

noteNames :: [String]
noteNames = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]

midiToName :: Int -> String
midiToName n = safeIndex noteNames (n `mod` 12) "C" ++ show ((n `div` 12) - 1)

midiNotePart :: Int -> String
midiNotePart n = safeIndex noteNames (n `mod` 12) "C"

midiOctavePart :: Int -> String
midiOctavePart n = show ((n `div` 12) - 1)

-- VU-meter bar: ▐████████░░░░▌
vuBar :: Int -> Int -> String
vuBar w v =
    let filled = clamp 0 w
            (round (fromIntegral v / 127.0 * fromIntegral w :: Double) :: Int)
    in  "▐" ++ replicate filled '█' ++ replicate (w - filled) '░' ++ "▌"

-- ─────────────────────────────────────────────────────────────────────────────
-- Waveform renderers
-- ─────────────────────────────────────────────────────────────────────────────

sparkLine :: [Double] -> String
sparkLine samples =
    let chars  = " ▁▂▃▄▅▆▇█"
        toIdx x = clamp 0 8 (round (abs x * 8.0) :: Int)
    in  map (\x -> safeIndex chars (toIdx x) ' ') samples

-- Multi-row dot-plot oscilloscope — CRT phosphor aesthetic.
waveformRows :: [Double] -> Int -> [String]
waveformRows samples height =
    let w      = min 64 (length samples)
        samps  = take w samples
        toRow x = clamp 0 (height - 1)
                     (round ((x + 1.0) / 2.0 * fromIntegral (height - 1)) :: Int)
        rowOf  = map toRow samps
        mkLine r = [ if rv == r then '●' else '·' | (_, rv) <- zip [0 :: Int ..] rowOf ]
    in  [ mkLine r | r <- [height - 1, height - 2 .. 0] ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Signal quality
-- ─────────────────────────────────────────────────────────────────────────────

confidenceBar :: Double -> String
confidenceBar c =
    let w      = 20
        filled = clamp 0 w (round (clamp 0.0 1.0 c * fromIntegral w) :: Int)
    in  replicate filled '█' ++ replicate (w - filled) '░'

confidenceAttr :: Double -> AttrName
confidenceAttr c
    | c >= 0.75 = highConfAttr
    | c >= 0.40 = midConfAttr
    | otherwise = lowConfAttr

latencyInfo :: Double -> (String, String, AttrName)
latencyInfo lat
    | lat == 0.0 = ("○", "no data",   dimAttr)
    | lat <  5.0 = ("●", "excellent", highConfAttr)
    | lat < 15.0 = ("●", "good",      midConfAttr)
    | lat < 30.0 = ("◐", "moderate",  midConfAttr)
    | otherwise  = ("○", "high",      lowConfAttr)

-- ─────────────────────────────────────────────────────────────────────────────
-- Tuning
-- ─────────────────────────────────────────────────────────────────────────────

tuningInTuneThreshold :: Double
tuningInTuneThreshold = 5.0

-- 41-char bar; 1 char ≈ 2.5 ¢; range ±50 ¢.
-- ◀ fills leftward (flat); ▶ fills rightward (sharp).
centsBar :: Double -> String
centsBar cents =
    let total  = 41
        centre = total `div` 2
        offset = clamp (-centre) centre (round (cents / 2.5) :: Int)
        pos    = centre + offset
        lChar  = if offset < 0 then '◀' else '░'
        rChar  = if offset > 0 then '▶' else '░'
    in  replicate pos lChar ++ "┃" ++ replicate (total - 1 - pos) rChar

-- Cents deviation from equal temperament.
-- NOTE: requires deFrequency :: Double (Hz) on DetectionEvent.
centsFromFreq :: Int -> Double -> Double
centsFromFreq midiNote freqHz
    | freqHz <= 0.0 = 0.0
    | otherwise     =
        let fRef = 440.0 * (2.0 ** (fromIntegral (midiNote - 69) / 12.0))
        in  1200.0 * logBase 2 (freqHz / fRef)

-- ─────────────────────────────────────────────────────────────────────────────
-- Border styles
-- ─────────────────────────────────────────────────────────────────────────────

-- Heavy double-line outer frame ╔═╗ ║ ╠═╣ ╚═╝ — the retro chrome.
retroBorder :: BorderStyle
retroBorder = BorderStyle
    { bsCornerTL      = '╔'
    , bsCornerTR      = '╗'
    , bsCornerBL      = '╚'
    , bsCornerBR      = '╝'
    , bsIntersectFull = '╬'
    , bsIntersectL    = '╠'
    , bsIntersectR    = '╣'
    , bsIntersectT    = '╦'
    , bsIntersectB    = '╩'
    , bsHorizontal    = '═'
    , bsVertical      = '║'
    }

-- Light rounded inner panels ╭─╮ │ ╰─╯ — contrast with outer double-line.
innerBorder :: BorderStyle
innerBorder = unicodeRounded

-- ─────────────────────────────────────────────────────────────────────────────
-- App  (attr map is dynamic: palette changes without restart)
-- ─────────────────────────────────────────────────────────────────────────────

tuiApp :: App TUIState TUIEvent ()
tuiApp = App
    { appDraw         = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent  = handleEvent
    , appStartEvent   = return ()
    , appAttrMap      = \s -> paletteMap (tuiPalette s)
    }

-- ─────────────────────────────────────────────────────────────────────────────
-- Top-level draw dispatcher
-- ─────────────────────────────────────────────────────────────────────────────

drawUI :: TUIState -> [Widget ()]
drawUI state = case tuiMode state of
    StartMode     -> [drawStartMenu state]
    MainMode      -> [drawMainUI   state]
    SoundFontMode -> [drawSoundFontUI state]

-- ─────────────────────────────────────────────────────────────────────────────
-- START MENU
-- ─────────────────────────────────────────────────────────────────────────────
--
--  ╔══════════ ◈  D·E·M·O·D·N·O·T·E  ◈ ══════════╗
--  ║                                               ║
--  ║   ╭──────────────────────────────────────╮   ║
--  ║   │  ★  D · E · M · O · D · N · O · T · E │  ║
--  ║   │     Detection · Modulation · Notation  │  ║
--  ║   ╰──────────────────────────────────────╯   ║
--  ║                                               ║
--  ║  ── T I P ──────────────────────────────      ║
--  ║  Sustain each note for 2+ beats…             ║
--  ║                                               ║
--  ║  ── J A C K  S E T U P ────────────────       ║
--  ║  ① Start jackd or QjackCtl first…            ║
--  ║  ② Select audio interface as JACK driver…    ║
--  ║  ③ Buffer size 64–256 samples…               ║
--  ║  ④ Connect instrument → capture port…        ║
--  ║  ⑤ Press ENTER when routing is confirmed…    ║
--  ║                                               ║
--  ║  ── C O L O R  T H E M E ──────────────       ║
--  ║  ► [1] Synthwave  [2] Sunset  [3] Matrix      ║
--  ║    [4] Ice        [5] Amber                   ║
--  ║                                               ║
--  ║         ▶  P R E S S  E N T E R  ◀            ║
--  ║                                               ║
--  ╚═══════════════════════════════════════════════╝

drawStartMenu :: TUIState -> Widget ()
drawStartMenu state =
    withBorderStyle retroBorder $
    borderWithLabel menuTitleW $
    padAll 2 $
    vBox
        [ logoBlock state
        , str " "
        , sectionHeader state "T I P"
        , padLeft (Pad 2) $ withAttr splashBodyAttr (str (tuiTip state))
        , str " "
        , sectionHeader state "J A C K  S E T U P"
        , vBox [ padLeft (Pad 2) $ withAttr splashBodyAttr (str l) | l <- jackGuide ]
        , str " "
        , sectionHeader state "C O L O R  T H E M E"
        , str " "
        , paletteSelector state
        , str " "
        , hCenter (pressEnterWidget state)
        , str " "
        ]
  where
    menuTitleW = withAttr titleAttr $
                 str (" ◈  " ++ intersperse '·' "DeMoD-NOTE" ++ "  ◈ ")

-- ── Animated logo block ───────────────────────────────────────────────────────

logoBlock :: TUIState -> Widget ()
logoBlock state =
    withBorderStyle innerBorder $
    borderWithLabel logoLabel $
    padAll 1 $
    vBox
        [ hCenter $ withAttr splashTitleAttr $
            str (orn ++ "  " ++ intersperse ' ' "DeMoD-NOTE" ++ "  " ++ orn)
        , hCenter $ withAttr splashSubAttr   $
            str "Detection  ·  Modulation  ·  Notation  Engine"
        ]
  where
    -- Ornament cycles through 4 glyphs, giving a slow shimmer.
    ornaments = ["◈", "◇", "◆", "◇"] :: [String]
    orn       = safeIndex ornaments (tuiTick state `mod` 4) "◈"
    logoLabel = withAttr dimAttr $ str " v1.0 "

-- ── Section divider header ────────────────────────────────────────────────────

sectionHeader :: TUIState -> String -> Widget ()
sectionHeader state label =
    padBottom (Pad 1) $
    withAttr splashDivAttr $
    str ("  ─── " ++ label ++ " " ++ replicate (max 0 (44 - length label)) '─')

-- ── Palette selector ──────────────────────────────────────────────────────────

paletteSelector :: TUIState -> Widget ()
paletteSelector state =
    vBox
        [ padLeft (Pad 2) $ hBox $ intersperse (str "  ") $
            map (paletteOption state) (take 3 allPalettes)
        , padLeft (Pad 2) $ hBox $ intersperse (str "  ") $
            map (paletteOption state) (drop 3 allPalettes)
        ]

paletteOption :: TUIState -> ColorPalette -> Widget ()
paletteOption state p =
    let selected = tuiPalette state == p
        pfx      = if selected then "► " else "  "
        atr      = if selected then accentAttr else dimAttr
    in  withAttr atr $
        str (pfx ++ "[" ++ [paletteKey p] ++ "] " ++ paletteName p)

-- ── JACK status indicator on startup screen ──────────────────────────────────

jackStatusWidget :: TUIState -> Widget ()
jackStatusWidget state =
    let (statusText, statusAttr) = case tuiJackStatus state of
            JackConnected    -> ("● JACK CONNECTED  ─  PRESS ENTER", jackGoodAttr)
            JackDisconnected -> ("○ WAITING FOR JACK...", jackBadAttr)
            JackReconnecting -> ("◐ RECONNECTING...", jackWarnAttr)
            JackError msg    -> ("○ JACK ERROR: " ++ take 20 msg, jackBadAttr)
        blinkOn = tuiTick state `mod` 2 == 0
        atr     = if tuiJackStatus state == JackConnected 
                  then if blinkOn then accentAttr else statusAttr
                  else statusAttr
    in  withAttr atr $ str statusText

pressEnterWidget :: TUIState -> Widget ()
pressEnterWidget state =
    let jackReady = tuiJackStatus state == JackConnected
        blinkOn   = tuiTick state `mod` 2 == 0
        atr       = if jackReady 
                    then if blinkOn then accentAttr else jackGoodAttr
                    else dimAttr
        msg       = if jackReady 
                    then "▶   P R E S S   E N T E R   ◀"
                    else "      W A I T I N G   F O R   J A C K      "
    in  withAttr atr $ str msg

-- ─────────────────────────────────────────────────────────────────────────────
-- MAIN UI
-- ─────────────────────────────────────────────────────────────────────────────
--
--  ╔═════════ ◈  D·E·M·O·D·N·O·T·E  ◈ ═════════╗
--  ║  J A C K  ║  T A P  B P M  ║  S I G N A L ║
--  ╠═══════════╩═════════════════╩══════════════╣
--  ║  ╭──────────── L A S T  N O T E ─────────╮ ║
--  ║  │  G#  4  ·  midi 68  ·  vel ▐████░▌  84│ ║
--  ║  ╰──────────────────────────────────────╯ ║
--  ╠══════════════════════════════════════════╣
--  ║  ╭─ W A V E F O R M  ▶ RUNNING ─────────╮ ║
--  ║  │  ▁▂▃▄▅▆▇▇▆▅▄▃▂▁▁▂▃▄                 │ ║
--  ║  │  ·●···●·●··············●···●··●·······│ ║
--  ║  ╰──────────────────────────────────────╯ ║
--  ╠══════════════════════════════════════════╣
--  ║  S C A L E   C Major  [1/11]  [s]›  [S]‹  ║
--  ║  A R P E G   None     [1/ 8]  [a]›  [A]‹  ║
--  ╠══════════════════════════════════════════╣
--  ║  H I S T   G#4·84  A4·72  F3·50  C5·99   ║
--  ╠══════════════════════════════════════════╣
--  ║  ◈  READY — JACK CONNECTED                ║
--  ║  [SPC]tap · [s/S]scale · [c]theme · [q]  ║
--  ╚══════════════════════════════════════════╝

drawMainUI :: TUIState -> Widget ()
drawMainUI state =
    withBorderStyle retroBorder $
    borderWithLabel (withAttr titleAttr $ str mainTitle) $
    vBox
        [ topRow     state
        , hBorder
        , heroPanel  state
        , hBorder
        , if tuiTuningMode state
            then tuningPanel   state
            else waveformPanel state
        , hBorder
        , presetRow  state
        , hBorder
        , historyRow state
        , hBorder
        , statusRow  state
        , mainHelpBar
        ]
  where
    mainTitle = " ◈  " ++ intersperse '·' "DeMoD-NOTE" ++ "  ◈ "

-- ── Top row: J A C K  │  T A P  B P M  │  S I G N A L ───────────────────────

topRow :: TUIState -> Widget ()
topRow state = hBox
    [ jackPanel   state
    , vBorder
    , bpmPanel    state
    , vBorder
    , signalPanel state
    ]

jackPanel :: TUIState -> Widget ()
jackPanel state = padAll 1 $ hLimit 22 $
    vBox
        [ withAttr dimAttr (str (spaced "JACK"))
        , hBox [ withAttr ja (str dot), str " ", withAttr ja (str lbl) ]
        ]
  where
    ja         = jackAttr (tuiJackStatus state)
    (dot, lbl) = jackDisplay (tuiJackStatus state)

jackDisplay :: JackStatus -> (String, String)
jackDisplay JackConnected    = ("●", "CONNECTED")
jackDisplay JackDisconnected = ("○", "DISCONNECTED")
jackDisplay JackReconnecting = ("◐", "RECONNECTING")
jackDisplay (JackError msg)  = ("○", "ERR: " ++ take 9 msg)

jackAttr :: JackStatus -> AttrName
jackAttr JackConnected    = jackGoodAttr
jackAttr JackDisconnected = jackBadAttr
jackAttr JackReconnecting = jackWarnAttr
jackAttr (JackError _)    = jackBadAttr

bpmPanel :: TUIState -> Widget ()
bpmPanel state = padAll 1 $ hLimit 24 $
    vBox
        [ withAttr dimAttr (str (spaced "TAP BPM"))
        , withAttr bpmAttr (str (spaced bpmStr))
        , hBox
            [ withAttr highConfAttr (str tapsOn)
            , withAttr dimAttr      (str tapsOff)
            , withAttr dimAttr      (str "  [SPC]")
            ]
        ]
  where
    bpmStr   = show (round (tuiBPM state) :: Int)
    tapCount = length (tuiTapTimes state)
    tapsOn   = replicate tapCount '●'
    tapsOff  = replicate (max 0 (8 - tapCount)) '○'

signalPanel :: TUIState -> Widget ()
signalPanel state = padAll 1 $
    vBox
        [ withAttr dimAttr (str (spaced "SIGNAL"))
        , hBox
            [ withAttr dimAttr            (str "CONF  ")
            , withAttr (confidenceAttr c) (str (confidenceBar c))
            , withAttr (confidenceAttr c) (str (" " ++ show pct ++ "%  " ++ confGlyph c))
            ]
        , hBox
            [ withAttr dimAttr   (str "LAT   ")
            , withAttr latAttr   (str latDot)
            , str " "
            , withAttr valueAttr (str (showFixed1 lat ++ " ms"))
            , withAttr dimAttr   (str ("  " ++ latLbl))
            ]
        ]
  where
    c                         = tuiConfidence state
    pct                       = round (c * 100) :: Int
    lat                       = tuiLatency state
    (latDot, latLbl, latAttr) = latencyInfo lat
    confGlyph v
        | v >= 0.75 = "● strong"
        | v >= 0.40 = "◐ moderate"
        | otherwise = "○ weak"

-- ── Hero panel ────────────────────────────────────────────────────────────────

heroPanel :: TUIState -> Widget ()
heroPanel state =
    withBorderStyle innerBorder $
    borderWithLabel (withAttr dimAttr $ str " L A S T  N O T E ") $
    padAll 1 $ padLeftRight 2 $
    case tuiLastNote state of
        Nothing ->
            -- Animate the waiting glyph using tick.
            let waitGlyphs = ["○", "◌", "○", "◎"] :: [String]
                glyph      = safeIndex waitGlyphs (tuiTick state `mod` 4) "○"
            in  hBox [ withAttr dimAttr (str (glyph ++ "  ─  awaiting input")) ]
        Just (n, v) ->
            hBox
                [ withAttr heroNoteAttr (str (midiNotePart n))
                , withAttr heroOctAttr  (str (midiOctavePart n))
                , withAttr dimAttr      (str "   ·   midi ")
                , withAttr valueAttr    (str (show n))
                , withAttr dimAttr      (str "   ·   vel ")
                , withAttr velAttr      (str (vuBar 12 v))
                , withAttr dimAttr      (str ("  " ++ show v))
                ]

-- ── Waveform panel ───────────────────────────────────────────────────────────

waveformPanel :: TUIState -> Widget ()
waveformPanel state =
    withBorderStyle innerBorder $
    borderWithLabel waveLabel $
    padTopBottom 1 $ padLeftRight 2 $
    vBox
        [ withAttr waveAttr (str (sparkLine wave))
        , str " "
        , vBox (map (withAttr waveAttr . str) (waveformRows wave 5))
        ]
  where
    wave    = tuiWaveform state
    runAttr = if tuiRunning state then highConfAttr else midConfAttr
    runStr  = if tuiRunning state then "▶ RUNNING" else "⏸ PAUSED  [p] resume"
    waveLabel = hBox
        [ withAttr dimAttr (str " W A V E F O R M  ")
        , withAttr runAttr (str runStr)
        , withAttr dimAttr (str " ")
        ]

-- ── Tuning panel ─────────────────────────────────────────────────────────────

tuningPanel :: TUIState -> Widget ()
tuningPanel state =
    withBorderStyle innerBorder $
    borderWithLabel
        (withAttr dimAttr $ str " C H R O M A T I C  T U N E R  ─  [t] exit ") $
    padAll 1 $ padLeftRight 2 $
    vBox [ noteRow, str " ", centsRow, str " ", statusWidget ]
  where
    ta    = if tuiTuningInTune state then tuningGoodAttr else tuningBadAttr
    cents = tuiTuningCents state

    noteRow = case tuiTuningNote state of
        Nothing -> withAttr dimAttr (str "○  play a note to begin tuning")
        Just n  -> hBox
            [ withAttr dimAttr      (str "N O T E  ")
            , withAttr heroNoteAttr (str (midiToName n))
            ]

    cSign   = if cents >= 0 then "+" else ""
    centsRow = vBox
        [ withAttr ta (str (centsBar cents))
        , hBox
            [ withAttr dimAttr (str "  ◄ flat")
            , withAttr ta      (str ("   " ++ cSign ++ show (round cents :: Int) ++ " ¢   "))
            , withAttr dimAttr (str "sharp ►")
            ]
        ]

    (dot, msg) = tuneStatus (tuiTuningInTune state) cents
    statusWidget = hBox [ withAttr ta (str dot), withAttr ta (str msg) ]

tuneStatus :: Bool -> Double -> (String, String)
tuneStatus True  _     = ("★", "  I N  T U N E")
tuneStatus False cents
    | abs cents <= 15.0 = ("◐", "  CLOSE  ─  adjust slightly")
    | cents < 0         = ("○", "  FLAT   ─  tune up ↑")
    | otherwise         = ("○", "  SHARP  ─  tune down ↓")

-- ── Preset row ───────────────────────────────────────────────────────────────

presetRow :: TUIState -> Widget ()
presetRow state = hBox [ scalePanel state, vBorder, arpeggioPanel state ]

scalePanel :: TUIState -> Widget ()
scalePanel state = padAll 1 $
    hBox
        [ withAttr dimAttr   (str (spaced "SCALE" ++ "  "))
        , withAttr valueAttr (str (currentScale state))
        , withAttr dimAttr   (str ("  [" ++ show (tuiScaleIndex state + 1)
            ++ "/" ++ show (length availableScales) ++ "]"))
        , withAttr dimAttr   (str "   [s]›  [S]‹")
        ]

arpeggioPanel :: TUIState -> Widget ()
arpeggioPanel state = padAll 1 $
    hBox
        [ withAttr dimAttr   (str (spaced "ARPEG" ++ "  "))
        , withAttr valueAttr (str (currentArpeggio state))
        , withAttr dimAttr   (str ("  [" ++ show (tuiArpeggioIndex state + 1)
            ++ "/" ++ show (length availableArpeggios) ++ "]"))
        , withAttr dimAttr   (str "   [a]›  [A]‹")
        ]

-- ── History row ──────────────────────────────────────────────────────────────

historyRow :: TUIState -> Widget ()
historyRow state = padAll 1 $
    hBox
        [ withAttr dimAttr (str (spaced "HIST" ++ "  "))
        , withAttr dimAttr (str histStr)
        ]
  where
    hist    = tuiNoteHistory state
    histStr
        | null hist = "─  no notes yet  ─"
        | otherwise = intercalate "  ╌  "
            [ midiToName n ++ "·" ++ show v | (n, v) <- take 10 hist ]

-- ── Status + help bars ───────────────────────────────────────────────────────

statusRow :: TUIState -> Widget ()
statusRow state = padLeftRight 1 $
    hBox
        [ withAttr accentAttr (str "◈  ")
        , withAttr statusAttr (str (tuiStatusMessage state))
        ]

mainHelpBar :: Widget ()
mainHelpBar = withAttr helpAttr $ padLeftRight 1 $
    str $ intercalate "  ·  "
        [ "[SPC] tap", "[s/S] scale", "[a/A] arpeg"
        , "[t] tuner", "[f] font", "[c] theme", "[p] pause", "[r] reset", "[q] quit"
        ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Event handling
-- ─────────────────────────────────────────────────────────────────────────────

handleEvent :: BrickEvent () TUIEvent -> EventM () TUIState ()

-- ── Animation tick (both modes) ───────────────────────────────────────────────
handleEvent (AppEvent TUITick) =
    modify' $ \s -> s { tuiTick = tuiTick s + 1 }

-- ── Start menu ───────────────────────────────────────────────────────────────
handleEvent ev = do
    mode <- gets tuiMode
    case mode of
        StartMode     -> handleStartEvent ev
        MainMode      -> handleMainEvent  ev
        SoundFontMode -> handleSoundFontEvent ev

handleStartEvent :: BrickEvent () TUIEvent -> EventM () TUIState ()
handleStartEvent (VtyEvent e) = case e of
    EvKey (KChar 'q') [] -> halt
    EvKey KEsc         [] -> halt

    -- ENTER: transition to main UI (only when JACK is connected)
    EvKey KEnter [] -> do
        jackReady <- gets (tuiJackStatus)
        when (jackReady == JackConnected) $ modify' $ \s ->
            s { tuiMode          = MainMode
              , tuiStatusMessage = "JACK CONNECTED  ─  AWAITING INPUT"
              }

    -- 1–5: select palette (live preview — attr map is dynamic)
    EvKey (KChar c) [] | Just p <- paletteFromChar c ->
        modify' $ \s ->
            s { tuiPalette = p
              , tuiStatusMessage = "THEME: " ++ paletteName p
              }

    _ -> return ()

-- Handle detection events in StartMode to update JACK status
handleStartEvent (AppEvent (TUIDetection det)) = modify' $ \s ->
    s { tuiJackStatus = deJackStatus det }

handleStartEvent _ = return ()

handleMainEvent :: BrickEvent () TUIEvent -> EventM () TUIState ()
handleMainEvent (VtyEvent e) = case e of
    EvKey (KChar 'q') [] -> halt
    EvKey KEsc         [] -> halt

    EvKey (KChar ' ') [] -> do
        now <- liftIO getCurrentTime
        modify' $ \s ->
            let taps   = take 8 (now : tuiTapTimes s)
                newBPM = computeBPM taps
            in  s { tuiTapTimes      = taps
                  , tuiBPM           = newBPM
                  , tuiStatusMessage = "TAP " ++ show (length taps)
                                    ++ " / 8  ─  " ++ show (round newBPM :: Int) ++ " BPM"
                  }

    EvKey (KChar 's') [] -> cycleScale    1
    EvKey (KChar 'S') [] -> cycleScale  (-1)
    EvKey (KChar 'a') [] -> cycleArpeggio    1
    EvKey (KChar 'A') [] -> cycleArpeggio  (-1)

    -- Cycle color palette (in-session)
    EvKey (KChar 'c') [] -> modify' $ \s ->
        let palettes = allPalettes
            idx      = length (takeWhile (/= tuiPalette s) palettes)
            next     = safeIndex palettes ((idx + 1) `mod` length palettes) Synthwave
        in  s { tuiPalette     = next
              , tuiStatusMessage = "THEME: " ++ paletteName next
              }

    -- Or pick palette directly by number
    EvKey (KChar c) [] | Just p <- paletteFromChar c ->
        modify' $ \s ->
            s { tuiPalette     = p
              , tuiStatusMessage = "THEME: " ++ paletteName p
              }

    EvKey (KChar 'p') [] -> modify' $ \s ->
        let r = not (tuiRunning s)
        in  s { tuiRunning = r, tuiStatusMessage = if r then "RUNNING" else "PAUSED" }

    EvKey (KChar 'r') [] -> modify' $ \s ->
        s { tuiTapTimes = [], tuiBPM = 120.0
          , tuiStatusMessage = "TAP TEMPO RESET  ─  120 BPM"
          }

    EvKey (KChar 't') [] -> modify' $ \s ->
        let m = not (tuiTuningMode s)
        in  s { tuiTuningMode    = m
              , tuiStatusMessage = if m then "TUNER ON  ─  PLAY A NOTE" else "TUNER OFF"
              }

    -- Open SoundFont browser
    EvKey (KChar 'f') [] -> modify' $ \s ->
        s { tuiMode = SoundFontMode }

    _ -> return ()

handleMainEvent (AppEvent (TUIStatusMsg msg)) =
    modify' $ \s -> s { tuiStatusMessage = msg }

-- Detection event: always update signal metrics; tuning only on note-on.
handleMainEvent (AppEvent (TUIDetection det)) = modify' $ \s ->
    let mNote = deNote det
        hist' = case mNote of
            Nothing     -> tuiNoteHistory s
            Just (n, v) -> take 10 ((n, v) : tuiNoteHistory s)
        wave' = take 64 (deWaveform det ++ repeat 0.0)
    in  s { tuiLastNote      = mNote
          , tuiNoteHistory   = hist'
          , tuiConfidence    = deConfidence det
          , tuiLatency       = deLatency    det
          , tuiWaveform      = wave'
          , tuiTuningNote    = deTuningNote det
          , tuiTuningCents   = deTuningCents det
          , tuiTuningInTune  = deTuningInTune det
          , tuiJackStatus    = deJackStatus det
          }

handleMainEvent _ = return ()

-- ── SoundFont mode event handler ─────────────────────────────────────────────

handleSoundFontEvent :: BrickEvent () TUIEvent -> EventM () TUIState ()
handleSoundFontEvent (VtyEvent e) = case e of
    EvKey KEsc         [] -> modify' $ \s -> s { tuiMode = MainMode }
    EvKey (KChar 'q')  [] -> modify' $ \s -> s { tuiMode = MainMode }
    EvKey KEnter       [] -> do
        -- Open browser to musical-artifacts.com for donation
        _ <- liftIO $ System.Process.spawnCommand 
            "xdg-open https://musical-artifacts.com 2>/dev/null || open https://musical-artifacts.com 2>/dev/null || echo 'Please visit https://musical-artifacts.com'"
        modify' $ \s -> s 
            { tuiMode = MainMode
            , tuiStatusMessage = "SOUNDFONT: Visit musical-artifacts.com to download!"
            }
    _ -> return ()
handleSoundFontEvent _ = return ()

-- ─────────────────────────────────────────────────────────────────────────────
-- SoundFont UI
-- ─────────────────────────────────────────────────────────────────────────────

drawSoundFontUI :: TUIState -> Widget ()
drawSoundFontUI state =
    withBorderStyle retroBorder $
    borderWithLabel (withAttr titleAttr $ str sfTitle) $
    padAll 2 $
    vBox
        [ hCenter $ withAttr splashTitleAttr $ str "★  S O U N D F O N T  ★"
        , str " "
        , hCenter $ withAttr splashSubAttr $ str "Download SoundFonts from"
        , str " "
        , hCenter $ withAttr valueAttr $ str "musical-artifacts.com"
        , str " "
        , donationBanner
        , str " "
        , hCenter $ withAttr splashBodyAttr $ str "SoundFonts will be saved to:"
        , str " "
        , hCenter $ withAttr dimAttr $ str "/etc/demod/sf"
        , hCenter $ withAttr dimAttr $ str "~/.local/share/soundfonts"
        , str " "
        , hCenter $ withAttr dimAttr $ str "Press [Enter] to open in browser"
        , str " "
        , hCenter $ pressEscapeWidget state
        ]
  where
    sfTitle = " ◈  SOUNDFONT  ◈ "
    donationBanner = withBorderStyle innerBorder $
        border $ padAll 1 $ vBox
            [ hCenter $ withAttr heroNoteAttr $ str "PLEASE SUPPORT THIS SERVICE!"
            , str " "
            , hCenter $ withAttr splashBodyAttr $ 
                str "musical-artifacts.com is run by volunteers."
            , hCenter $ withAttr splashBodyAttr $ 
                str "Consider donating to keep it running."
            ]

pressEscapeWidget :: TUIState -> Widget ()
pressEscapeWidget state =
    let blinkOn = tuiTick state `mod` 2 == 0
        atr     = if blinkOn then accentAttr else dimAttr
    in  withAttr atr $ str "[Esc] Back  ·  [Enter] Open Browser"

-- ─────────────────────────────────────────────────────────────────────────────

cycleScale :: Int -> EventM () TUIState ()
cycleScale dir = modify' $ \s ->
    let n = (tuiScaleIndex s + dir) `mod` length availableScales
    in  s { tuiScaleIndex    = n
          , tuiStatusMessage = "SCALE  ─  " ++ safeIndex availableScales n "?"
          }

cycleArpeggio :: Int -> EventM () TUIState ()
cycleArpeggio dir = modify' $ \s ->
    let n = (tuiArpeggioIndex s + dir) `mod` length availableArpeggios
    in  s { tuiArpeggioIndex = n
          , tuiStatusMessage = "ARPEGGIO  ─  " ++ safeIndex availableArpeggios n "?"
          }

computeBPM :: [UTCTime] -> Double
computeBPM []  = 120.0
computeBPM [_] = 120.0
computeBPM ts  =
    let pairs     = zip ts (safeTail ts)
        intervals = map (\(a, b) -> realToFrac (diffUTCTime a b) :: Double) pairs
        avg       = sum intervals / fromIntegral (length intervals)
    in  clamp 20.0 300.0 (60.0 / avg)

-- ─────────────────────────────────────────────────────────────────────────────
-- Attribute names
-- ─────────────────────────────────────────────────────────────────────────────

-- Structural / chrome
titleAttr, valueAttr, dimAttr, bpmAttr   :: AttrName
velAttr, waveAttr, helpAttr              :: AttrName
accentAttr, statusAttr                   :: AttrName
-- Signal quality
highConfAttr, midConfAttr, lowConfAttr   :: AttrName
-- Hero note
heroNoteAttr, heroOctAttr               :: AttrName
-- Tuning
tuningGoodAttr, tuningBadAttr           :: AttrName
-- JACK
jackGoodAttr, jackBadAttr, jackWarnAttr :: AttrName
-- Splash / start menu
splashTitleAttr, splashSubAttr          :: AttrName
splashDivAttr, splashBodyAttr           :: AttrName
-- Brick progress bar (required by ProgressBar widget)
progressCompleteAttr, progressIncompleteAttr :: AttrName

titleAttr            = attrName "title"
valueAttr            = attrName "value"
dimAttr              = attrName "dim"
bpmAttr              = attrName "bpm"
velAttr              = attrName "vel"
waveAttr             = attrName "wave"
helpAttr             = attrName "help"
accentAttr           = attrName "accent"
statusAttr           = attrName "status"
highConfAttr         = attrName "highConf"
midConfAttr          = attrName "midConf"
lowConfAttr          = attrName "lowConf"
heroNoteAttr         = attrName "heroNote"
heroOctAttr          = attrName "heroOct"
tuningGoodAttr       = attrName "tuningGood"
tuningBadAttr        = attrName "tuningBad"
jackGoodAttr         = attrName "jackGood"
jackBadAttr          = attrName "jackBad"
jackWarnAttr         = attrName "jackWarn"
splashTitleAttr      = attrName "splashTitle"
splashSubAttr        = attrName "splashSub"
splashDivAttr        = attrName "splashDiv"
splashBodyAttr       = attrName "splashBody"
progressCompleteAttr   = attrName "progressComplete"
progressIncompleteAttr = attrName "progressIncomplete"

-- ─────────────────────────────────────────────────────────────────────────────
-- Entry points
-- ─────────────────────────────────────────────────────────────────────────────

runTUI :: Config -> IO ()
runTUI cfg = runTUIWithChannel cfg Nothing

-- | Launch the TUI.  Always uses customMain (even with no backend channel)
--   so the animation tick thread can drive start-menu effects.
--
--   Bug fixed: previously defaultMain was always used, silently dropping
--   backend detection events when a BChan was constructed.
runTUIWithChannel :: Config -> Maybe (Chan DetectionEvent) -> IO ()
runTUIWithChannel cfg mChan = do
    -- Pick a pseudo-random tip based on seconds since epoch.
    posix <- getPOSIXTime
    let tipIdx = (floor posix :: Int) `mod` length tips
        tip    = safeIndex tips tipIdx "Enjoy the music."
        initSt = initialTUIState cfg tip

    bc <- newBChan 100

    -- Animation tick: fires every 280 ms → ~3.6 ticks/sec.
    -- Drives logo shimmer and "press enter" pulse on start screen,
    -- and the hero panel waiting glyph on the main screen.
    void $ forkIO $ forever $ do
        threadDelay 280000
        writeBChan bc TUITick

    -- Optional: backend detection event bridge.
    case mChan of
        Nothing   -> return ()
        Just chan  -> void $ forkIO $ forever $ do
            ev <- readChan chan
            writeBChan bc (TUIDetection ev)

    let buildVty = mkVty defaultConfig
    initVty <- buildVty
    void $ customMain initVty buildVty (Just bc) tuiApp initSt
