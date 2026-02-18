{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import DeMoDNote.Config
import DeMoDNote.Backend
import DeMoDNote.OSC
import DeMoDNote.Monitor
import DeMoDNote.Types
import DeMoDNote.TUI (runTUI)
import DeMoDNote.Preset (getPresetByName, listPresets)
import DeMoDNote.Scale (getScaleByName, allScaleNames)
import DeMoDNote.Arpeggio (createArpeggio, majorChord, upPattern)
import qualified DeMoDNote.BPM as BPM
import Katip
import Control.Concurrent.Async
import Control.Concurrent.STM
import System.Exit
import System.IO (stdout)
import qualified Data.Text as T

data Command
  = Run { cmdConfig :: Maybe FilePath, cmdPreset :: Maybe String, cmdInteractive :: Bool }
  | TestScale { cmdScale :: String, cmdBPM :: Double, cmdDuration :: Int }
  | TestArpeggio { cmdRoot :: String, cmdQuality :: String, cmdPattern :: String, cmdBPM :: Double }
  | ListPresets
  | ListScales
  | ShowPreset { cmdPresetName :: String }
  | TUI
  deriving (Show)

optsParser :: Parser Command
optsParser = subparser
  ( command "run" (info runCmd (progDesc "Run the note detector"))
    <> command "test-scale" (info testScaleCmd (progDesc "Test a scale"))
    <> command "test-arpeggio" (info testArpeggioCmd (progDesc "Test an arpeggio"))
    <> command "list-presets" (info listPresetsCmd (progDesc "List available presets"))
    <> command "list-scales" (info listScalesCmd (progDesc "List available scales"))
    <> command "show-preset" (info showPresetCmd (progDesc "Show preset details"))
    <> command "tui" (info tuiCmd (progDesc "Launch interactive TUI"))
  )

runCmd :: Parser Command
runCmd = Run
  <$> optional (strOption (long "config" <> short 'c' <> metavar "FILE" <> help "Config file"))
  <*> optional (strOption (long "preset" <> short 'p' <> metavar "NAME" <> help "Preset name"))
  <*> switch (long "interactive" <> short 'i' <> help "Interactive TUI mode")

testScaleCmd :: Parser Command
testScaleCmd = TestScale
  <$> argument str (metavar "SCALE" <> help "Scale name (e.g., 'C Major')")
  <*> option auto (long "bpm" <> value 120.0 <> help "Tempo in BPM")
  <*> option auto (long "duration" <> short 'd' <> value 10 <> help "Duration in seconds")

testArpeggioCmd :: Parser Command
testArpeggioCmd = TestArpeggio
  <$> argument str (metavar "ROOT" <> help "Root note (e.g., 'C')")
  <*> argument str (metavar "QUALITY" <> help "Chord quality (major, minor, dom7, etc.)")
  <*> argument str (metavar "PATTERN" <> help "Pattern (up, down, up-down, broken-3, etc.)")
  <*> option auto (long "bpm" <> value 120.0 <> help "Tempo in BPM")

listPresetsCmd :: Parser Command
listPresetsCmd = pure ListPresets

listScalesCmd :: Parser Command
listScalesCmd = pure ListScales

showPresetCmd :: Parser Command
showPresetCmd = ShowPreset
  <$> argument str (metavar "NAME" <> help "Preset name")

tuiCmd :: Parser Command
tuiCmd = pure TUI

main :: IO ()
main = do
  cmd <- execParser $ info (optsParser <**> helper) 
    (fullDesc <> progDesc "DeMoDNote - Deterministic Monophonic Note Detector")
  
  case cmd of
    Run mCfg mPreset useTUI -> 
      if useTUI
      then runWithTUI mCfg
      else runNormal mCfg mPreset
    
    TestScale scale bpm duration -> do
      putStrLn $ "Testing scale: " ++ scale ++ " at " ++ show bpm ++ " BPM"
      case getScaleByName scale of
        Nothing -> putStrLn $ "Unknown scale: " ++ scale
        Just s -> putStrLn $ "Scale: " ++ show s
      putStrLn "(Scale test playback would start here)"
    
    TestArpeggio root quality pattern bpm -> do
      putStrLn $ "Testing arpeggio: " ++ root ++ " " ++ quality ++ " " ++ pattern
      putStrLn $ "Tempo: " ++ show bpm ++ " BPM"
      -- Would create arpeggio and play it
      putStrLn "(Arpeggio test playback would start here)"
    
    ListPresets -> do
      presets <- listPresets
      putStrLn "Available presets:"
      mapM_ putStrLn presets
    
    ListScales -> do
      putStrLn "Available scales (showing first 20):"
      mapM_ putStrLn (take 20 allScaleNames)
    
    ShowPreset name -> do
      mPreset <- getPresetByName name
      case mPreset of
        Nothing -> putStrLn $ "Preset not found: " ++ name
        Just preset -> putStrLn $ show preset
    
    TUI -> do
      cfg <- loadConfig Nothing
      runTUI cfg

runNormal :: Maybe FilePath -> Maybe String -> IO ()
runNormal mCfg mPreset = do
  cfg <- loadConfig mCfg
  
  -- Apply preset if specified
  cfg' <- case mPreset of
    Nothing -> return cfg
    Just presetName -> do
      putStrLn $ "Loading preset: " ++ presetName
      return cfg  -- Would apply preset
  
  state <- newTVarIO (emptyReactorState cfg')
  le <- initLogEnv "demod-note" "production"
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  logEnv' <- registerScribe "stdout" handleScribe defaultScribeSettings le

  oscAsync <- async $ startOSC (oscPort cfg') state
  monitorAsync <- async $ startMonitor (monitorPort cfg') state
  backendAsync <- async $ runBackend cfg' state

  putStrLn "DeMoDNote started. Press Ctrl+C to stop."
  waitAnyCatchCancel [oscAsync, monitorAsync, backendAsync]
  closeScribes logEnv'
  exitSuccess

runWithTUI :: Maybe FilePath -> IO ()
runWithTUI mCfg = do
  cfg <- loadConfig mCfg
  putStrLn "Starting in TUI mode..."
  runTUI cfg
