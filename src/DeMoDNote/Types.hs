module DeMoDNote.Types where

import DeMoDNote.Config
import Data.Word (Word64)

type MIDINote = Int
type Velocity = Int
type TimeStamp = Word64  -- Microseconds since epoch

data NoteState
  = Idle
  | Attacking TimeStamp                    -- Onset detected, estimating
  | FastNote MIDINote Velocity TimeStamp   -- Output at 2.66ms (200Hz+)
  | MediumNote MIDINote Velocity TimeStamp -- Output at 12ms (bass speculative)
  | Validated MIDINote Velocity            -- Confirmed at 30ms
  | Releasing TimeStamp                    -- Note ending
  deriving (Show, Eq)

data OnsetFeatures = OnsetFeatures
  { spectralFlux   :: {-# UNPACK #-} !Double
  , energyTransient:: {-# UNPACK #-} !Double
  , phaseDeviation :: {-# UNPACK #-} !Double
  , combinedScore  :: {-# UNPACK #-} !Double
  } deriving (Show, Eq)

defaultOnsetFeatures :: OnsetFeatures
defaultOnsetFeatures = OnsetFeatures 0.0 0.0 0.0 0.0

data PLLState = PLLState
  { pllFrequency :: {-# UNPACK #-} !Double
  , pllPhase     :: {-# UNPACK #-} !Double
  , pllLocked    :: {-# UNPACK #-} !Bool
  , pllConfidence:: {-# UNPACK #-} !Double
  , pllLastCrossing :: {-# UNPACK #-} !Int
  } deriving (Show, Eq)

defaultPLLState :: PLLState
defaultPLLState = PLLState 0.0 0.0 False 0.0 0

data DetectionResult = DetectionResult
  { detectedNote :: Maybe (MIDINote, Velocity)
  , confidence   :: {-# UNPACK #-} !Double
  , noteState    :: !NoteState
  , needsBend    :: Maybe (MIDINote, Double)  -- (targetNote, bendAmountSemitones)
  } deriving (Show)

data ReactorState = ReactorState
  { currentNotes    :: [(MIDINote, Velocity)]
  , noteStateMach   :: !NoteState
  , pllStateMach    :: !PLLState
  , onsetFeatures   :: !OnsetFeatures
  , lastOnsetTime   :: !TimeStamp
  , config          :: !Config
  } deriving (Show)

emptyReactorState :: Config -> ReactorState
emptyReactorState cfg = ReactorState
  { currentNotes = []
  , noteStateMach = Idle
  , pllStateMach = defaultPLLState
  , onsetFeatures = defaultOnsetFeatures
  , lastOnsetTime = 0
  , config = cfg
  }

-- MIDI Event with timing
data MidiEvent = MidiEvent
  { eventType   :: !MidiEventType
  , eventNote   :: !MIDINote
  , eventVel    :: !Velocity
  , eventTime   :: !TimeStamp
  , eventBend   :: Maybe Double  -- Pitch bend in semitones
  } deriving (Show, Eq)

data MidiEventType = NoteOn | NoteOff | PitchBend
  deriving (Show, Eq, Ord, Enum, Bounded)

-- Frequency ranges for hybrid detection
fastPathMinFreq :: Double
fastPathMinFreq = 200.0

mediumPathMinFreq :: Double
mediumPathMinFreq = 80.0

bassMinFreq :: Double
bassMinFreq = 30.0

-- Sample counts for different validation windows
samplesAt96kHz :: Double -> Int
samplesAt96kHz ms = round (ms * 96.0)  -- 96kHz / 1000 = 96 samples per ms

fastWindowSamples :: Int
fastWindowSamples = samplesAt96kHz 2.66  -- ~256 samples

mediumWindowSamples :: Int
mediumWindowSamples = samplesAt96kHz 12.0  -- ~1152 samples

slowWindowSamples :: Int
slowWindowSamples = samplesAt96kHz 30.0  -- ~2880 samples
