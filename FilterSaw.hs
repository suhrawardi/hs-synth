{-# LANGUAGE NoImplicitPrelude #-}
module FilterSaw (reverbedSignal) where

import qualified Synthesizer.Plain.Control    as Ctrl
import qualified Synthesizer.Plain.Oscillator as Osci
import qualified Synthesizer.Plain.Filter.Recursive.Comb as Comb
import qualified Synthesizer.Plain.Filter.Recursive.Allpass as Allpass
import qualified Synthesizer.Plain.Signal as Sig
import qualified Synthesizer.Plain.File as File
import qualified Synthesizer.Plain.Filter.Recursive.Universal as UniFilter
import qualified Synthesizer.Plain.Filter.Recursive as FiltR

import qualified Haskore.Basic.Pitch as Pitch
import qualified Haskore.Melody          as Melody
import           Haskore.Melody.Standard as StdMelody
import qualified Haskore.Music           as Music
import           Haskore.Music.Standard  as StdMusic
import qualified Haskore.Performance.Fancy as FancyPf
import qualified Haskore.Interface.Signal.Note  as Note
import qualified Haskore.Interface.Signal.Write as MusicSignal
import Haskore.Interface.Signal.Write(Time,Volume)

import System.Random (randomRs, mkStdGen, )
-- import Algebra.Additive as Additive(zero)
import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module         as Module

import System.Exit(ExitCode)
import qualified Data.List as List

import NumericPrelude.Base hiding (take, )
import NumericPrelude.Numeric

------------ The song description -------------

filterSaw sampleRate filterFreq freq =
    map (\r -> UniFilter.lowpass r * 0.1)
        (UniFilter.run (map (UniFilter.parameter . FiltR.Pole 10)
                        (Ctrl.exponential2 (0.1*sampleRate) (filterFreq/sampleRate)))
                   (Osci.staticSine 0 (freq/sampleRate)))


data MyNote = FilterSaw Double Pitch.T
   deriving (Show, Eq, Ord)

type Resonance = Time

pattern :: Music.T MyNote
pattern = makePattern patternList (Osci.staticSine 0 0.1)

makePattern :: [Music.Dur -> Resonance -> Melody.T Resonance] ->
   [Resonance] -> Music.T MyNote
makePattern mel reson = line
   (zipWith (\n r -> Music.mapNote
                        (\(Melody.Note r' p) -> (FilterSaw r' p))
                        (n qn r))
            mel reson)

patternList :: [Music.Dur -> Resonance -> Melody.T Resonance]
patternList = concatMap (List.take 16 . cycle) (
   [[c 2, b 4, b 4, c 3],
    [c 4, c 6, c 2, b 4],
    [c 5, b 4, b 4, c 3],
    [c 4, c 6, c 5, b 4],
    [c 2, b 4, b 4, c 3],
    [c 4, c 6, c 2, b 4],
    [c 5, b 4, b 4, c 3],
    [c 4, c 6, c 5, b 4],
    [c 2, b 4, b 4, c 3],
    [c 4, c 6, c 2, b 4],
    [c 6, c 5, c 6, b 5],
    [c 5, b 4, b 4, c 3],
    [c 4, c 6, c 5, b 4],
    [c 6, c 5, c 6, b 5],
    [c 2, b 4, b 4, c 3],
    [c 4, c 6, c 2, b 4],
    [c 6, c 5, c 6, b 5],
    [c 5, b 4, b 4, c 3],
    [c 4, c 6, c 5, b 4],
    [c 6, c 5, c 6, b 5],
    [c 5, b 5, b 5, c 5]])


----------- Configuration of the player -----------


noteToSignal ::
   Time -> Volume -> Pitch.Relative -> MyNote -> Note.T Volume Volume
noteToSignal detune _ trans (FilterSaw reso p) =
   Note.Cons (\sampleRate ->
      filterSaw sampleRate ((2000::Time)  *  2 ** (0.5*reso))
         (detune * Note.pitchFromStd trans p))

-- Volume type arises from Haskore
songToSignalMono :: Time -> Volume -> Music.T MyNote -> Sig.T Volume
songToSignalMono sampleRate dif song =
   MusicSignal.fromMusic
      sampleRate (noteToSignal dif)
      FancyPf.map
      (MusicSignal.contextMetro 120 qn)
      song

songSignal :: Time -> Sig.T Volume
songSignal sampleRate = songToSignalMono sampleRate 1 pattern

allpassChannel sampleRate sign x =
   let order = 10
   in  Allpass.cascade order
          (map ((\(Allpass.Parameter k) -> Allpass.Parameter (sign*k)) .
                (Allpass.flangerParameter order))
               (Ctrl.exponential2 (3*sampleRate) (200/sampleRate))
           ) x

stereoSignal :: Time -> Sig.T (Volume,Volume)
stereoSignal sampleRate =
   zip (allpassChannel sampleRate ( 1 :: Volume) (songSignal sampleRate))
       (allpassChannel sampleRate (-1 :: Volume) (songSignal sampleRate))

reverbedSignal :: Time -> Sig.T (Volume,Volume)
reverbedSignal = stereoSignal
