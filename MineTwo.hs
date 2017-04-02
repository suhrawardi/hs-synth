{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module MineTwo (stereoSignal) where

import qualified Synthesizer.Plain.Control    as Ctrl
import qualified Synthesizer.Plain.Oscillator as Osci
import qualified Synthesizer.Plain.Filter.Recursive.Allpass as Allpass
import qualified Synthesizer.Plain.Signal as Sig

import qualified Haskore.Basic.Pitch as Pitch
import qualified Haskore.Melody          as Melody
import           Haskore.Melody.Standard as StdMelody
import qualified Haskore.Music           as Music
import           Haskore.Music.Standard  as StdMusic
import qualified Haskore.Performance.Fancy as FancyPf
import qualified Haskore.Interface.Signal.Note  as Note
import qualified Haskore.Interface.Signal.Write as MusicSignal
import Haskore.Interface.Signal.Write(Time,Volume)

import Synthesizer.Plain.Instrument as Instr

import System.Random (randomRs, mkStdGen, )
-- import Algebra.Additive as Additive(zero)

import System.Exit(ExitCode)
import qualified Data.List as List

import NumericPrelude.Base hiding (take, )
import NumericPrelude.Numeric

data MyNote = MineTwo Double Pitch.T
  deriving (Show, Eq, Ord)

type Resonance = Time

sampleRate = 44100::Double

pattern :: String -> Music.T MyNote
pattern "noise" = makePattern patternListNoise (Osci.staticSaw 0 0.01)
pattern "bell" = makePattern patternListBell (Osci.staticSaw 0 0.0001)

makePattern :: [Music.Dur -> Resonance -> Melody.T Resonance] ->
   [Resonance] -> Music.T MyNote
makePattern mel reson = line
   (zipWith (\n r -> Music.mapNote
                        (\(Melody.Note r' p) -> (MineTwo r' p))
                        (n qn r))
            mel reson)

patternListBell :: [Music.Dur -> Resonance -> Melody.T Resonance]
patternListBell = concatMap (List.take 160 . cycle) (
   [[c 1, b 2, b 1, c 2],
    [c 1, b 2, b 1, c 2]])

patternListNoise :: [Music.Dur -> Resonance -> Melody.T Resonance]
patternListNoise = concatMap (List.take 16 . cycle) (
   [[c 5, b 4, b 4, c 6],
    [c 6, b 4, b 4, c 6],
    [c 5, b 4, b 4, c 6],
    [c 6, b 4, b 4, c 6],
    [c 6, c 5, c 6, b 5],
    [c 5, b 4, b 4, c 3],
    [c 4, c 6, c 5, b 4],
    [c 6, c 5, c 6, b 5],
    [c 7, b 4, b 4, c 3],
    [c 4, c 6, c 7, b 4],
    [c 6, c 5, c 6, b 5],
    [c 5, b 4, b 4, c 3],
    [c 4, c 6, c 5, b 4],
    [c 6, c 5, c 6, b 5],
    [c 5, b 5, b 5, c 5]])

noteToSignal ::
   String -> Time -> Volume -> Pitch.Relative -> MyNote -> Note.T Volume Volume
noteToSignal "noise" detune _ trans (MineTwo reso p) =
   Note.Cons (\sampleRate ->
      Instr.noiseBass sampleRate (detune * Note.pitchFromStd trans p))
noteToSignal "bell" detune _ trans (MineTwo reso p) =
   Note.Cons (\sampleRate ->
      Instr.bell sampleRate (detune * Note.pitchFromStd trans p))

songToSignalMono :: String -> Time -> Volume -> Music.T MyNote -> Sig.T Volume
songToSignalMono key time dif song =
   MusicSignal.fromMusic
      sampleRate (noteToSignal key dif)
      FancyPf.map
      (MusicSignal.contextMetro time qn)
      song

songSignal :: String -> Time -> Sig.T Volume
songSignal key time = songToSignalMono key time 1 $ pattern key

stereoSignal :: Sig.T (Volume,Volume)
stereoSignal =
   zip (allpassChannel ( 1 :: Volume) (songSignal "noise" 40))
       (allpassChannel (-1 :: Volume) (songSignal "bell" 120))

allpassChannel sign x =
   let order = 10
   in  Allpass.cascade order
          (map ((\(Allpass.Parameter k) -> Allpass.Parameter (sign*k)) .
                (Allpass.flangerParameter order))
               (Ctrl.exponential2 (3*sampleRate) (200/sampleRate))
           ) x
