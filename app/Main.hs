{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import DeMoDNote.Config
import DeMoDNote.Backend
import DeMoDNote.OSC
import DeMoDNote.Monitor
import DeMoDNote.Types
import DeMoDNote.TUI (runTUI, runTUIWithChannel, runTUIWithState)
import DeMoDNote.Preset (getPresetByName, listPresets, applyPreset)
import DeMoDNote.SoundFont (initSoundFontManager, startFluidSynth, isFluidSynthRunning, SoundFontManager)
import DeMoDNote.Recording (initRecording, startRecording, stopRecording, flushRecording)
import DeMoDNote.Scale (getScaleByName, allScaleNames)
import DeMoDNote.Arpeggio (createArpeggio, majorChord, upPattern)
import qualified DeMoDNote.BPM as BPM
#ifdef FlagOpenGL
import DeMoDNote.Opengl (runOpenGLVisualizer, runOpenGLStandalone, OpenGLConfig(..), defaultOpenGLConfig)
#endif
import Katip
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent (forkIO, forkOS, threadDelay, readChan, newChan, writeChan, Chan)
import Data.Maybe (fromMaybe)
import System.Exit
import System.IO (stdout)
import qualified Data.Text as T
import Control.Exception (catch, SomeException)

data Command
  = Run { cmdConfig :: Maybe FilePath, cmdPreset :: Maybe String, cmdInteractive :: Bool, cmdSynth :: Maybe FilePath, cmdRecord :: Maybe FilePath }
  | TestScale { cmdScale :: String, cmdBPM :: Double, cmdDuration :: Int }
  | TestArpeggio { cmdRoot :: String, cmdQuality :: String, cmdPattern :: String, cmdBPM :: Double }
  | ListPresets
  | ListScales
  | ShowPreset { cmdPresetName :: String }
  | TUI
  | OpenGL { cmdMidiFile :: Maybe FilePath, cmdObjFile :: Maybe FilePath, cmdSvgFile :: Maybe FilePath }
  | OpenGLWithBackend { cmdConfig :: Maybe FilePath }
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
    <> command "opengl" (info openglCmd (progDesc "Launch OpenGL visualizer (standalone)"))
    <> command "opengl-backend" (info openglBackendCmd (progDesc "Launch OpenGL visualizer with JACK backend"))
  )

openglCmd :: Parser Command
openglCmd = OpenGL
  <$> optional (strOption (long "midi" <> short 'm' <> metavar "FILE" <> help "MIDI file for falling notes"))
  <*> optional (strOption (long "obj" <> short 'o' <> metavar "FILE" <> help "OBJ 3D model file"))
  <*> optional (strOption (long "svg" <> short 's' <> metavar "FILE" <> help "SVG file for rendering"))

openglBackendCmd :: Parser Command
openglBackendCmd = OpenGLWithBackend
  <$> optional (strOption (long "config" <> short 'c' <> metavar "FILE" <> help "Config file"))

runCmd :: Parser Command
runCmd = Run
  <$> optional (strOption (long "config" <> short 'c' <> metavar "FILE" <> help "Config file"))
  <*> optional (strOption (long "preset" <> short 'p' <> metavar "NAME" <> help "Preset name"))
  <*> switch (long "interactive" <> short 'i' <> help "Interactive TUI mode")
  <*> optional (strOption (long "synth" <> short 's' <> metavar "FILE" <> help "SoundFont file for FluidSynth audio output"))
  <*> optional (strOption (long "record" <> short 'r' <> metavar "FILE" <> help "Record session to file (protobuf+csv)"))

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
    Run mCfg mPreset useTUI mSynth mRecord -> 
      if useTUI
      then runWithTUI mCfg
      else runNormal mCfg mPreset mSynth mRecord
    
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
      state <- newTVarIO (emptyReactorState cfg)
      putStrLn "Starting DeMoD-Note TUI with JACK backend..."
      backendAsync <- async $ runBackend cfg state Nothing
      runTUIWithState cfg state
      cancel backendAsync
    
#ifdef FlagOpenGL
    OpenGL mMidi mObj mSvg -> do
      putStrLn "Starting OpenGL visualizer (standalone mode)..."
      let oglCfg = defaultOpenGLConfig
            { oglMidiFile = mMidi
            , oglObjFile = mObj
            , oglSvgFile = mSvg
            }
      runOpenGLStandalone oglCfg
    
    OpenGLWithBackend mCfg -> do
      cfg <- loadConfig mCfg
      state <- newTVarIO (emptyReactorState cfg)
      putStrLn "Starting DeMoD-Note with OpenGL visualizer and JACK backend..."
      putStrLn "  • Backend running on audio thread"
      putStrLn "  • OpenGL running on dedicated render thread"
      putStrLn "Press ESC in OpenGL window to quit"
      
      -- Start backend services in background
      backendAsync <- async $ runBackend cfg state Nothing
      
      -- Run OpenGL on dedicated OS thread (required for GL context)
      -- forkOS binds the thread to a specific OS thread for OpenGL
      runOpenGLVisualizer defaultOpenGLConfig state
      
      -- Cleanup
      cancel backendAsync
#else
    OpenGL _ _ _ -> do
      putStrLn "OpenGL support not compiled in."
      putStrLn "Rebuild with -f opengl flag to enable OpenGL visualization."
    
    OpenGLWithBackend _ -> do
      putStrLn "OpenGL support not compiled in."
      putStrLn "Rebuild with -f opengl flag to enable OpenGL visualization."
#endif

runNormal :: Maybe FilePath -> Maybe String -> Maybe FilePath -> Maybe FilePath -> IO ()
runNormal mCfg mPreset mSynth mRecord = do
  cfg <- loadConfig mCfg
  
  -- Apply preset if specified
  cfg' <- case mPreset of
    Nothing -> return cfg
    Just presetName -> do
      putStrLn $ "Loading preset: " ++ presetName
      mPreset <- getPresetByName presetName
      case mPreset of
        Nothing -> do
          putStrLn $ "Warning: Preset not found: " ++ presetName
          return cfg
        Just preset -> do
          let cfgWithPreset = applyPreset preset cfg
          putStrLn $ "Applied preset: " ++ presetName
          putStrLn $ "  Detection: onset=" ++ show (onsetThresh $ detection cfgWithPreset) ++ 
                      " fast=" ++ show (fastValidationMs $ detection cfgWithPreset) ++ "ms"
          return cfgWithPreset
  
  -- Initialize FluidSynth if soundfont specified
  synthManager <- case mSynth of
    Nothing -> do
      putStrLn "FluidSynth: disabled (no --synth option)"
      return Nothing
    Just sfPath -> do
      putStrLn $ "FluidSynth: initializing with " ++ sfPath
      manager <- initSoundFontManager
      manager' <- startFluidSynth manager sfPath
      running <- isFluidSynthRunning manager'
      if running
        then do
          putStrLn "FluidSynth: started successfully"
          return (Just manager')
        else do
          putStrLn "FluidSynth: failed to start"
          return Nothing
  
  -- Initialize recording if file specified
  recordingState <- case mRecord of
    Nothing -> do
      putStrLn "Recording: disabled (no --record option)"
      return Nothing
    Just recPath -> do
      putStrLn $ "Recording: will save to " ++ recPath
      let presetName = fromMaybe "default" mPreset
      rec <- initRecording (T.pack presetName)
      rec' <- startRecording rec
      return (Just rec')
  
  state <- newTVarIO (emptyReactorState cfg')
  le <- initLogEnv "demod-note" "production"
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  logEnv' <- registerScribe "stdout" handleScribe defaultScribeSettings le

  let handleAsyncError e = do
        putStrLn $ "Async error: " ++ show e
        putStrLn "Attempting to clean up..."
  
  oscAsync <- async $ catch (startOSC (oscPort cfg') state) $ \e -> do
        handleAsyncError (e :: SomeException)
        return ()
  monitorAsync <- async $ catch (startMonitor (monitorPort cfg') state) $ \e -> do
        handleAsyncError (e :: SomeException)
        return ()
  backendAsync <- async $ catch (runBackend cfg' state synthManager) $ \e -> do
        handleAsyncError (e :: SomeException)
        return ()

  putStrLn "DeMoDNote started. Press Ctrl+C to stop."
  result <- waitAnyCatchCancel [oscAsync, monitorAsync, backendAsync]
  case result of
    (_, Left e) -> do
        putStrLn $ "Error in main loop: " ++ show e
    _ -> return ()
  
  -- Flush and save recording if active
  case (mRecord, recordingState) of
    (Just recPath, Just rec) -> do
      putStrLn $ "Saving recording to " ++ recPath
      flushRecording rec recPath
    _ -> return ()
  
  closeScribes logEnv'
  exitSuccess

runWithTUI :: Maybe FilePath -> IO ()
runWithTUI mCfg = do
  cfg <- loadConfig mCfg
  state <- newTVarIO (emptyReactorState cfg)
  
  putStrLn "Starting DeMoD-Note TUI with JACK backend..."
  
  -- Start backend services (JACK, OSC, monitor) in background
  backendAsync <- async $ runBackend cfg state Nothing
  
  -- Run TUI with backend state connection
  runTUIWithState cfg state
  
  -- Cleanup
  cancel backendAsync
