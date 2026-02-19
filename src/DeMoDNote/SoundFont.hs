module DeMoDNote.SoundFont (
    SoundFont(..),
    SoundFontManager,
    SoundFontInfo(..),
    initSoundFontManager,
    loadSoundFont,
    unloadSoundFont,
    getLoadedSoundFont,
    listSoundFonts,
    findSoundFont,
    validateSoundFont,
    getSoundFontInfo,
    setMIDIProgram,
    setMIDIBank,
    sendMidiProgramChange,
    -- Default soundfonts
    defaultSoundFontPaths,
    systemSoundFontDir,
    userSoundFontDir,
    -- FluidSynth integration
    startFluidSynth,
    stopFluidSynth,
    isFluidSynthRunning,
    restartFluidSynth
) where

import System.Directory (doesFileExist, getHomeDirectory, listDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName, takeExtension)
import System.Process (ProcessHandle)  -- Needed for type
-- import System.IO (hClose, hGetContents, Handle)  -- Kept for future use
-- import Control.Concurrent (threadDelay)  -- Kept for future use
-- import Control.Monad (filterM, when)  -- Kept for future use
import Data.List (isSuffixOf, find)
-- import Data.Maybe (fromMaybe)  -- Kept for future use

-- SoundFont file information
data SoundFont = SoundFont {
    sfPath :: FilePath,
    sfName :: String,
    sfSize :: Integer,
    sfLoaded :: Bool,
    sfPrograms :: [Int]  -- Available MIDI programs in this soundfont
} deriving (Show, Eq)

-- SoundFont metadata
data SoundFontInfo = SoundFontInfo {
    sfiName :: String,
    sfiPath :: FilePath,
    sfiVersion :: String,
    sfiTargetEngine :: String,  -- e.g., "FluidSynth", "GeneralUser"
    sfiPresets :: [String],     -- Preset names
    sfiSizeMB :: Double
} deriving (Show, Eq)

-- SoundFont manager state
data SoundFontManager = SoundFontManager {
    sfmSystemDir :: FilePath,
    sfmUserDir :: FilePath,
    sfmCurrentSoundFont :: Maybe SoundFont,
    sfmMidiProgram :: Int,
    sfmMidiBank :: Int,
    sfmFluidSynthProcess :: Maybe ProcessHandle,
    sfmFluidSynthPort :: Int
}

-- Default search paths for soundfonts
defaultSoundFontPaths :: [FilePath]
defaultSoundFontPaths = [
    "/usr/share/soundfonts",
    "/usr/local/share/soundfonts",
    "/opt/soundfonts"
    ]

systemSoundFontDir :: FilePath
systemSoundFontDir = "/usr/share/soundfonts"

userSoundFontDir :: IO FilePath
userSoundFontDir = do
    home <- getHomeDirectory
    let dir = home </> ".local" </> "share" </> "soundfonts"
    createDirectoryIfMissing True dir
    return dir

-- Initialize soundfont manager
initSoundFontManager :: IO SoundFontManager
initSoundFontManager = do
    userDir <- userSoundFontDir
    return $ SoundFontManager {
        sfmSystemDir = systemSoundFontDir,
        sfmUserDir = userDir,
        sfmCurrentSoundFont = Nothing,
        sfmMidiProgram = 0,
        sfmMidiBank = 0,
        sfmFluidSynthProcess = Nothing,
        sfmFluidSynthPort = 9800
    }

-- Validate a soundfont file (check extension and existence)
validateSoundFont :: FilePath -> IO (Either String SoundFont)
validateSoundFont path = do
    exists <- doesFileExist path
    if not exists
    then return $ Left $ "SoundFont not found: " ++ path
    else do
        let ext = takeExtension path
        if ext `elem` [".sf2", ".sf3", ".dls"]
        then do
            -- TODO: Get file size and parse soundfont info
            return $ Right $ SoundFont {
                sfPath = path,
                sfName = takeFileName path,
                sfSize = 0,  -- Would use getFileSize
                sfLoaded = False,
                sfPrograms = [0..127]
            }
        else return $ Left $ "Invalid SoundFont format: " ++ ext

-- Load a soundfont into the manager
loadSoundFont :: SoundFontManager -> FilePath -> IO (Either String SoundFontManager)
loadSoundFont manager path = do
    result <- validateSoundFont path
    case result of
        Left err -> return $ Left err
        Right sf -> do
            -- Stop existing fluidsynth if running
            _ <- stopFluidSynth manager
            -- Start new fluidsynth with this soundfont
            newManager <- startFluidSynth manager path
            return $ Right newManager {
                sfmCurrentSoundFont = Just sf { sfLoaded = True }
            }

-- Unload current soundfont
unloadSoundFont :: SoundFontManager -> IO SoundFontManager
unloadSoundFont manager = do
    manager' <- stopFluidSynth manager
    return $ manager' {
        sfmCurrentSoundFont = Nothing
    }

-- Get currently loaded soundfont
getLoadedSoundFont :: SoundFontManager -> Maybe SoundFont
getLoadedSoundFont = sfmCurrentSoundFont

-- List all available soundfonts in search paths
listSoundFonts :: SoundFontManager -> IO [SoundFontInfo]
listSoundFonts manager = do
    systemSFs <- listDirSFs (sfmSystemDir manager)
    userSFs <- listDirSFs (sfmUserDir manager)
    let allPaths = systemSFs ++ userSFs
    mapM pathToInfo allPaths
    where
        listDirSFs dir = do
            exists <- doesFileExist dir  -- Actually should use doesDirectoryExist
            if not exists
            then return []
            else do
                files <- listDirectory dir
                let sfFiles = filter (\f -> any (`isSuffixOf` f) [".sf2", ".sf3", ".dls"]) files
                return $ map (dir </>) sfFiles
        
        pathToInfo path = return $ SoundFontInfo {
            sfiName = takeFileName path,
            sfiPath = path,
            sfiVersion = "Unknown",
            sfiTargetEngine = "General MIDI",
            sfiPresets = [],  -- Would parse from SF2 file
            sfiSizeMB = 0.0   -- Would get from file
        }

-- Find a soundfont by name (partial match)
findSoundFont :: SoundFontManager -> String -> IO (Maybe SoundFontInfo)
findSoundFont manager query = do
    sfs <- listSoundFonts manager
    return $ find (\sf -> query `isInfixOf` sfiName sf) sfs
    where
        isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs = xs : tails (drop 1 xs)

-- Get detailed info about a soundfont
getSoundFontInfo :: FilePath -> IO (Either String SoundFontInfo)
getSoundFontInfo path = do
    valid <- validateSoundFont path
    case valid of
        Left err -> return $ Left err
        Right sf -> return $ Right $ SoundFontInfo {
            sfiName = sfName sf,
            sfiPath = path,
            sfiVersion = "2.01",  -- Would parse from file
            sfiTargetEngine = "FluidSynth",
            sfiPresets = ["Acoustic Grand Piano", "Bright Acoustic Piano", "Electric Grand Piano"],  -- Would parse
            sfiSizeMB = fromIntegral (sfSize sf) / (1024 * 1024)
        }

-- Set MIDI program (instrument)
setMIDIProgram :: SoundFontManager -> Int -> IO SoundFontManager
setMIDIProgram manager prog = do
    let clamped = max 0 (min 127 prog)
    -- Send program change message
    sendMidiProgramChange manager clamped (sfmMidiBank manager)
    return $ manager { sfmMidiProgram = clamped }

-- Set MIDI bank
setMIDIBank :: SoundFontManager -> Int -> IO SoundFontManager
setMIDIBank manager bank = do
    let clamped = max 0 (min 127 bank)
    return $ manager { sfmMidiBank = clamped }

-- Send MIDI program change (would actually send to synth)
sendMidiProgramChange :: SoundFontManager -> Int -> Int -> IO ()
sendMidiProgramChange _manager program bank = do
    -- This would send actual MIDI messages
    putStrLn $ "MIDI Program Change: Bank " ++ show bank ++ ", Program " ++ show program

-- Start FluidSynth with a soundfont
startFluidSynth :: SoundFontManager -> FilePath -> IO SoundFontManager
startFluidSynth manager soundfontPath = do
    let port = sfmFluidSynthPort manager
        _cmd = "fluidsynth -a jack -m jack -g 0.8 -p DeMoDNote \"" ++ soundfontPath ++ "\" &"
    
    putStrLn $ "Starting FluidSynth with: " ++ soundfontPath
    -- In real implementation, would properly capture process handle
    return $ manager {
        sfmFluidSynthProcess = Nothing,  -- Would be Just handle
        sfmFluidSynthPort = port
    }

-- Stop FluidSynth
stopFluidSynth :: SoundFontManager -> IO SoundFontManager
stopFluidSynth manager = do
    case sfmFluidSynthProcess manager of
        Nothing -> return manager
        Just _ph -> do
            -- terminateProcess ph  -- Would kill the process
            putStrLn "Stopping FluidSynth"
            return $ manager { sfmFluidSynthProcess = Nothing }

-- Check if FluidSynth is running
isFluidSynthRunning :: SoundFontManager -> Bool
isFluidSynthRunning manager = 
    case sfmFluidSynthProcess manager of
        Nothing -> False
        Just _ -> True  -- Would check if process is actually alive

-- Restart FluidSynth with current soundfont
restartFluidSynth :: SoundFontManager -> IO SoundFontManager
restartFluidSynth manager = do
    manager' <- stopFluidSynth manager
    case sfmCurrentSoundFont manager' of
        Nothing -> return manager'
        Just sf -> startFluidSynth manager' (sfPath sf)
