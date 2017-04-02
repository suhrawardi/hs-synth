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

pattern :: Music.T MyNote
pattern = makePattern patternList (Osci.staticSaw 0 0.01)

makePattern :: [Music.Dur -> Resonance -> Melody.T Resonance] ->
   [Resonance] -> Music.T MyNote
makePattern mel reson = line
   (zipWith (\n r -> Music.mapNote
                        (\(Melody.Note r' p) -> (MineTwo r' p))
                        (n qn r))
            mel reson)

patternList :: [Music.Dur -> Resonance -> Melody.T Resonance]
patternList = concatMap (List.take 16 . cycle) (
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
   Time -> Volume -> Pitch.Relative -> MyNote -> Note.T Volume Volume
noteToSignal detune _ trans (MineTwo reso p) =
   Note.Cons (\sampleRate ->
      Instr.noiseBass sampleRate (detune * Note.pitchFromStd trans p))
--      Instr.filterSaw sampleRate ((2000::Time)  *  2 ** (0.5*reso))
--         (detune * Note.pitchFromStd trans p))

songToSignalMono :: Volume -> Music.T MyNote -> Sig.T Volume
songToSignalMono dif song =
   MusicSignal.fromMusic
      sampleRate (noteToSignal dif)
      FancyPf.map
      (MusicSignal.contextMetro 120 qn)
      song

songSignal :: Sig.T Volume
songSignal = songToSignalMono 1 pattern

stereoSignal :: Sig.T (Volume,Volume)
stereoSignal =
   zip (allpassChannel ( 1 :: Volume) songSignal)
       (allpassChannel (-1 :: Volume) songSignal)

allpassChannel sign x =
   let order = 10
   in  Allpass.cascade order
          (map ((\(Allpass.Parameter k) -> Allpass.Parameter (sign*k)) .
                (Allpass.flangerParameter order))
               (Ctrl.exponential2 (3*sampleRate) (200/sampleRate))
           ) x
