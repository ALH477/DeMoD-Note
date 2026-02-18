{-# LANGUAGE BangPatterns #-}

module DeMoDNote.Detector where

import qualified Data.Vector.Storable as VS
import Numeric.FFT.Vector.Invertible as FFT
import Data.Complex
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Word (Word64)
import DeMoDNote.Config
import DeMoDNote.Types

-- Sample rate configuration
detectorSampleRate :: Double
detectorSampleRate = 96000.0

detectorBufferSize :: Int
detectorBufferSize = 128

-- Convert Int16 samples to normalized Float
normalizeInt16 :: VS.Vector Int -> VS.Vector Double
normalizeInt16 = VS.map (\x -> fromIntegral x / 32768.0)

-- Hann window
hannWindow :: Int -> VS.Vector Double
hannWindow n = VS.generate n (\i -> 
  0.5 * (1 - cos (2 * pi * fromIntegral i / fromIntegral (n - 1))))

-- Calculate spectral flux (difference between current and previous spectrum)
calcSpectralFlux :: VS.Vector Double -> VS.Vector Double -> Double
calcSpectralFlux curr prev = 
  let diff = VS.zipWith (\c p -> max 0 (c - p)) curr prev
  in VS.sum diff

-- Calculate energy transient (RMS difference)
calcEnergyTransient :: VS.Vector Double -> VS.Vector Double -> Double
calcEnergyTransient curr prev =
  let currRms = sqrt $ VS.sum (VS.map (\x -> x * x) curr) / fromIntegral (VS.length curr)
      prevRms = sqrt $ VS.sum (VS.map (\x -> x * x) prev) / fromIntegral (VS.length prev)
  in abs (currRms - prevRms)

-- Calculate phase deviation from PLL
calcPhaseDeviation :: PLLState -> VS.Vector Double -> Double
calcPhaseDeviation pll samples =
  if not (pllLocked pll) 
  then 1.0  -- Maximum deviation when not locked
  else 
    let expectedPhase = pllPhase pll + (2 * pi * pllFrequency pll / detectorSampleRate) * fromIntegral (VS.length samples)
        actualZeroCross = findZeroCrossing samples
        phaseError = case actualZeroCross of
          Nothing -> 1.0
          Just zc -> abs (expectedPhase - (fromIntegral zc / fromIntegral (VS.length samples) * 2 * pi))
    in min 1.0 (phaseError / pi)  -- Normalize to 0-1

-- Find first zero crossing
findZeroCrossing :: VS.Vector Double -> Maybe Int
findZeroCrossing v
  | VS.length v < 2 = Nothing
  | otherwise = go 0
  where
    go i
      | i >= VS.length v - 1 = Nothing
      | v VS.! i < 0 && v VS.! (i+1) >= 0 = Just i
      | otherwise = go (i+1)

-- Update onset features
updateOnsetFeatures :: Config -> VS.Vector Double -> VS.Vector Double -> OnsetFeatures -> OnsetFeatures
updateOnsetFeatures cfg curr prev prevFeatures =
  let sf = calcSpectralFlux curr prev
      et = calcEnergyTransient curr prev
      -- Phase deviation calculated separately per-call
      oc = onset cfg
      combined = spectralWeight oc * sf + 
                 energyWeight oc * et +
                 phaseWeight oc * 0.5  -- Default phase contribution
  in OnsetFeatures sf et 0.5 combined

-- Detect onset (quantized)
detectOnset :: Config -> OnsetFeatures -> Bool
detectOnset cfg features = 
  combinedScore features > threshold (onset cfg)

-- PLL Update
updatePLL :: VS.Vector Double -> PLLState -> PLLState
updatePLL samples pll =
  case findZeroCrossing samples of
    Nothing -> pll { pllLocked = False, pllConfidence = pllConfidence pll * 0.9 }
    Just zc ->
      let samplesSinceLast = if pllLastCrossing pll == 0 
                             then 0 
                             else zc - pllLastCrossing pll
          instantFreq = if samplesSinceLast > 0
                        then detectorSampleRate / fromIntegral samplesSinceLast
                        else pllFrequency pll
          -- Low-pass filter frequency (alpha = 0.3)
          alpha = 0.3
          newFreq = alpha * instantFreq + (1 - alpha) * pllFrequency pll
          newPhase = pllPhase pll + (2 * pi * newFreq / detectorSampleRate) * fromIntegral detectorBufferSize
          conf = min 1.0 (pllConfidence pll * 0.9 + 0.1)
      in PLLState
        { pllFrequency = newFreq
        , pllPhase = newPhase `mod'` (2 * pi)
        , pllLocked = conf > 0.6
        , pllConfidence = conf
        , pllLastCrossing = zc
        }

-- Fast frequency detection (zero-crossing based)
detectFastFreq :: VS.Vector Double -> Maybe (Double, Double)  -- (freq, confidence)
detectFastFreq samples =
  let crossings = findAllZeroCrossings samples
      periods = zipWith (-) (tail crossings) crossings
  in if length periods < 2
     then Nothing
     else 
       let avgPeriod = fromIntegral (sum periods) / fromIntegral (length periods)
           freq = detectorSampleRate / avgPeriod
           variance = sum (map (\p -> let d = fromIntegral p - avgPeriod in d * d) periods) / fromIntegral (length periods)
           stability = 1.0 / (1.0 + variance / 100.0)
       in if freq >= fastPathMinFreq && freq <= 4000.0 && stability > 0.5
          then Just (freq, stability)
          else Nothing

findAllZeroCrossings :: VS.Vector Double -> [Int]
findAllZeroCrossings v = go 0
  where
    go i
      | i >= VS.length v - 1 = []
      | v VS.! i < 0 && v VS.! (i+1) >= 0 = i : go (i+1)
      | otherwise = go (i+1)

-- Medium frequency detection (autocorrelation)
detectMediumFreq :: VS.Vector Double -> Maybe (Double, Double)
detectMediumFreq samples =
  let n = min (VS.length samples) mediumWindowSamples
      buf = VS.take n samples
      -- Simple autocorrelation peak finding
      lagRange = [round (detectorSampleRate / 200) .. round (detectorSampleRate / 80)]  -- 80-200Hz
      autocorr lag = 
        let pairs = [(buf VS.! i, buf VS.! (i + lag)) | i <- [0..n-lag-1]]
            products = map (\(a, b) -> a * b) pairs
        in sum products
      peaks = [(lag, autocorr lag) | lag <- lagRange]
      bestPeak = case peaks of
        [] -> Nothing
        ps -> Just $ maximumBy (comparing snd) ps
  in case bestPeak of
       Nothing -> Nothing
       Just (lag, corr) -> 
         let freq = detectorSampleRate / fromIntegral lag
             strength = corr / fromIntegral n
         in if strength > 0.1
            then Just (freq, min 1.0 strength)
            else Nothing

-- Slow frequency detection (YIN)
detectSlowFreq :: VS.Vector Double -> Maybe (Double, Double)
detectSlowFreq samples =
  let n = min (VS.length samples) slowWindowSamples
      buf = VS.take n samples
      -- YIN difference function (simplified)
      tauMax = min n (floor (detectorSampleRate / bassMinFreq))
      yinDiff tau = 
        let diffs = [(buf VS.! i - buf VS.! (i + tau))^2 | i <- [0..n-tau-1]]
        in sum diffs / fromIntegral (n - tau)
      -- Find minimum below threshold
      search tau 
        | tau >= tauMax = Nothing
        | otherwise = 
            let diff = yinDiff tau
                thresh = 0.1
            in if diff < thresh
               then Just (tau, 1.0 - diff)
               else search (tau + 1)
  in case search (floor (detectorSampleRate / 200)) of
       Nothing -> Nothing
       Just (tau, conf) -> 
         let freq = detectorSampleRate / fromIntegral tau
         in Just (freq, conf)

-- Convert frequency to MIDI note
freqToMidi :: Double -> Int
freqToMidi freq = round $ 69 + 12 * logBase 2 (freq / 440.0)

-- Calculate pitch bend amount (semitones)
calcPitchBend :: Int -> Double -> Double  -- (currentNote, targetFreq) -> bendAmount
calcPitchBend currentNote targetFreq =
  let currentFreq = 440.0 * (2 ** ((fromIntegral currentNote - 69) / 12))
      ratio = targetFreq / currentFreq
      semitones = 12 * logBase 2 ratio
  in max (-2.0) (min 2.0 semitones)  -- Clamp to ±2 semitones

-- Main detection function with state machine
detect :: Config -> VS.Vector Int -> TimeStamp -> NoteState -> PLLState -> OnsetFeatures -> IO DetectionResult
detect cfg samplesInt16 currentTime prevState prevPLL prevOnset = do
  let samples = normalizeInt16 samplesInt16
      n = VS.length samples
  
  -- Update onset features
  let currDouble = VS.map realToFrac samples :: VS.Vector Double
      newOnset = updateOnsetFeatures cfg currDouble currDouble prevOnset  -- Simplified: use same for prev
  
  -- Update PLL
  let newPLL = updatePLL samples prevPLL
  
  -- Check for onset
  let isOnset = detectOnset cfg newOnset
  
  -- State machine
  case prevState of
    Idle -> 
      if isOnset
      then pure $ DetectionResult Nothing 0.0 (Attacking currentTime) Nothing
      else pure $ DetectionResult Nothing 0.0 Idle Nothing
      
    Attacking startTime ->
      let elapsedMs = fromIntegral (currentTime - startTime) / 1000.0
      in if elapsedMs >= bassConfirmMs (detection cfg)
         then -- Timeout - force detection with slow method
           case detectSlowFreq samples of
             Nothing -> pure $ DetectionResult Nothing 0.0 Idle Nothing
             Just (freq, conf) -> 
               let note = freqToMidi freq
                   vel = min 127 $ max 1 $ round (127 * conf)
               in pure $ DetectionResult (Just (note, vel)) conf (Validated note vel) Nothing
         else if elapsedMs >= bassFastMs (detection cfg)
         then -- Medium window - speculative bass output
           case detectMediumFreq samples of
             Nothing -> pure $ DetectionResult Nothing 0.0 prevState Nothing
             Just (freq, conf) -> 
               if conf >= bassFastConf (detection cfg)
               then 
                 let note = freqToMidi freq
                     vel = min 127 $ max 1 $ round (127 * conf)
                 in pure $ DetectionResult (Just (note, vel)) conf (MediumNote note vel currentTime) Nothing
               else pure $ DetectionResult Nothing 0.0 prevState Nothing
         else if elapsedMs >= fastValidationMs (detection cfg)
         then -- Fast window - high freq only
           case detectFastFreq samples of
             Nothing -> pure $ DetectionResult Nothing 0.0 prevState Nothing
             Just (freq, conf) -> 
               if conf >= highFreqConf (detection cfg) && freq >= fastPathMinFreq
               then 
                 let note = freqToMidi freq
                     vel = min 127 $ max 1 $ round (127 * conf)
                 in pure $ DetectionResult (Just (note, vel)) conf (FastNote note vel currentTime) Nothing
               else pure $ DetectionResult Nothing 0.0 prevState Nothing
         else pure $ DetectionResult Nothing 0.0 prevState Nothing
    
    FastNote note vel startTime ->
      -- Keep outputting until new onset or timeout
      if isOnset
      then pure $ DetectionResult (Just (note, vel)) 1.0 (Attacking currentTime) Nothing
      else pure $ DetectionResult (Just (note, vel)) 1.0 prevState Nothing
    
    MediumNote note vel startTime ->
      let elapsedMs = fromIntegral (currentTime - startTime) / 1000.0
      in if elapsedMs >= bassConfirmMs (detection cfg)
         then -- Confirm or correct
           case detectSlowFreq samples of
             Nothing -> pure $ DetectionResult (Just (note, vel)) 0.9 (Validated note vel) Nothing
             Just (freq, conf) -> 
               let targetNote = freqToMidi freq
               in if targetNote /= note && conf >= bassFinalConf (detection cfg)
                  then 
                    let bend = calcPitchBend note freq
                    in pure $ DetectionResult (Just (targetNote, vel)) conf (Validated targetNote vel) (Just (targetNote, bend))
                  else pure $ DetectionResult (Just (note, vel)) 0.95 (Validated note vel) Nothing
         else if isOnset
         then pure $ DetectionResult (Just (note, vel)) 0.9 (Attacking currentTime) Nothing
         else pure $ DetectionResult (Just (note, vel)) 0.9 prevState Nothing
    
    Validated note vel ->
      if isOnset
      then pure $ DetectionResult (Just (note, vel)) 1.0 (Attacking currentTime) Nothing
      else pure $ DetectionResult (Just (note, vel)) 1.0 prevState Nothing
    
    Releasing startTime ->
      let elapsedMs = fromIntegral (currentTime - startTime) / 1000.0
      in if elapsedMs > 50.0  -- 50ms release time
         then pure $ DetectionResult Nothing 0.0 Idle Nothing
         else pure $ DetectionResult Nothing 0.0 prevState Nothing

-- Utility
maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy cmp = foldl1 (\x y -> if cmp x y == GT then x else y)

mod' :: Double -> Double -> Double
mod' x y = x - y * fromIntegral (floor (x / y))
