{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : DeMoDNote.Config
-- Description : Configuration types and parsing for DeMoD-Note
-- Copyright   : 2026
-- License     : MIT
--
-- This module defines configuration data types for detection parameters,
-- real-time settings, and TOML configuration file parsing.

module DeMoDNote.Config where

import GHC.Generics
import Toml
import Control.Exception (Exception, throwIO)

data ConfigException = ConfigParseError String
                     | ConfigValidationError String
    deriving (Show)

instance Exception ConfigException

data BackendType = Jack | PortAudio deriving (Show, Eq, Generic, Enum, Bounded)

data DetectionConfig = DetectionConfig
  { algorithm        :: String
  , windowSize       :: Int
  , hopSize          :: Int
  , maxPolyphony     :: Int
  , onsetThresh      :: Double
  , fastValidationMs :: Double
  , bassFastMs       :: Double
  , bassConfirmMs    :: Double
  , pitchBendRange   :: Double
  , highFreqConf     :: Double
  , bassFastConf     :: Double
  , bassFinalConf    :: Double
  } deriving (Show, Generic)

data TimingConfig = TimingConfig
  { sampleRate     :: Int
  , bufferSize     :: Int
  , maxJitterUs    :: Int
  , quantizeGrid   :: Bool
  , fluidsynthComp :: Double
  } deriving (Show, Generic)

data RTConfig = RTConfig
  { singleCore     :: Bool
  , cpuCore        :: Int
  , priorityAudio  :: Int
  , priorityDetect :: Int
  , memoryLock     :: Bool
  } deriving (Show, Generic)

data OnsetConfig = OnsetConfig
  { spectralWeight :: Double
  , energyWeight   :: Double
  , phaseWeight    :: Double
  , threshold      :: Double
  , quantize       :: Bool
  } deriving (Show, Generic)

data Config = Config
  { backend       :: BackendType
  , detection     :: DetectionConfig
  , timing        :: TimingConfig
  , rt            :: RTConfig
  , onset         :: OnsetConfig
  , oscPort       :: Int
  , monitorPort   :: Int
  , logLevel      :: String
  , activePreset  :: String
  } deriving (Show, Generic)

defaultConfig :: Config
defaultConfig = Config
  { backend = Jack
  , detection = DetectionConfig
      { algorithm = "Hybrid"
      , windowSize = 2048
      , hopSize = 128
      , maxPolyphony = 1
      , onsetThresh = -40.0
      , fastValidationMs = 2.66
      , bassFastMs = 12.0
      , bassConfirmMs = 30.0
      , pitchBendRange = 2.0
      , highFreqConf = 0.98
      , bassFastConf = 0.85
      , bassFinalConf = 0.95
      }
  , timing = TimingConfig
      { sampleRate = 96000
      , bufferSize = 128
      , maxJitterUs = 500
      , quantizeGrid = True
      , fluidsynthComp = 5.0
      }
  , rt = RTConfig
      { singleCore = True
      , cpuCore = 2
      , priorityAudio = 99
      , priorityDetect = 98
      , memoryLock = True
      }
  , onset = OnsetConfig
      { spectralWeight = 0.4
      , energyWeight = 0.4
      , phaseWeight = 0.2
      , threshold = 0.67
      , quantize = True
      }
  , oscPort = 57120
  , monitorPort = 8080
  , logLevel = "Info"
  , activePreset = "default"
  }

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.enumBounded "backend" .= backend
  <*> Toml.table detectionCodec "detection" .= detection
  <*> Toml.table timingCodec "timing" .= timing
  <*> Toml.table rtCodec "rt" .= rt
  <*> Toml.table onsetCodec "onset" .= onset
  <*> Toml.int "osc.port" .= oscPort
  <*> Toml.int "monitor.port" .= monitorPort
  <*> Toml.string "logLevel" .= logLevel
  <*> Toml.string "preset" .= activePreset

detectionCodec :: TomlCodec DetectionConfig
detectionCodec = DetectionConfig
  <$> Toml.string "algorithm" .= algorithm
  <*> Toml.int "window" .= windowSize
  <*> Toml.int "hop" .= hopSize
  <*> Toml.int "maxPolyphony" .= maxPolyphony
  <*> Toml.double "onsetThreshDb" .= onsetThresh
  <*> Toml.double "fastValidationMs" .= fastValidationMs
  <*> Toml.double "bassFastMs" .= bassFastMs
  <*> Toml.double "bassConfirmMs" .= bassConfirmMs
  <*> Toml.double "pitchBendRange" .= pitchBendRange
  <*> Toml.double "highFreqConf" .= highFreqConf
  <*> Toml.double "bassFastConf" .= bassFastConf
  <*> Toml.double "bassFinalConf" .= bassFinalConf

timingCodec :: TomlCodec TimingConfig
timingCodec = TimingConfig
  <$> Toml.int "sampleRate" .= sampleRate
  <*> Toml.int "bufferSize" .= bufferSize
  <*> Toml.int "maxJitterUs" .= maxJitterUs
  <*> Toml.bool "quantizeGrid" .= quantizeGrid
  <*> Toml.double "fluidsynthComp" .= fluidsynthComp

rtCodec :: TomlCodec RTConfig
rtCodec = RTConfig
  <$> Toml.bool "singleCore" .= singleCore
  <*> Toml.int "cpuCore" .= cpuCore
  <*> Toml.int "priorityAudio" .= priorityAudio
  <*> Toml.int "priorityDetect" .= priorityDetect
  <*> Toml.bool "memoryLock" .= memoryLock

onsetCodec :: TomlCodec OnsetConfig
onsetCodec = OnsetConfig
  <$> Toml.double "spectralWeight" .= spectralWeight
  <*> Toml.double "energyWeight" .= energyWeight
  <*> Toml.double "phaseWeight" .= phaseWeight
  <*> Toml.double "threshold" .= threshold
  <*> Toml.bool "quantize" .= quantize

loadConfig :: Maybe FilePath -> IO Config
loadConfig Nothing = pure defaultConfig
loadConfig (Just p) = do
  result <- Toml.decodeFileExact configCodec p
  case result of
    Left errs -> throwIO $ ConfigParseError (show errs)
    Right cfg -> pure cfg
