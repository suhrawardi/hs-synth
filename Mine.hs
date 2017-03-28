{-# LANGUAGE FlexibleContexts #-}
module Mine (mOscillator1, mOscillator2, mOscillator3)
 where

import qualified Sound.Sox.Play as Play
import qualified Sound.Sox.Option.Format as SoxOpt
import qualified Synthesizer.Basic.Binary as BinSmp
import qualified Synthesizer.Storable.Signal as SigSt
import qualified Synthesizer.Generic.Signal as SigG
import qualified Signal2 as SigG2
import qualified Synthesizer.State.Signal as Sig
import qualified Synthesizer.Causal.Process as Causal
import Control.Arrow ((&&&), (^<<), (<<^), (<<<), )

import qualified Synthesizer.Generic.Oscillator as Osci
import qualified Synthesizer.Generic.Filter.NonRecursive as Filt
import qualified Synthesizer.Plain.Filter.Recursive as FiltRec
import qualified Synthesizer.Plain.Filter.Recursive.Universal as UniFilter
import qualified Synthesizer.Basic.Wave as Wave

import qualified Synthesizer.State.Control as CtrlS
import qualified Synthesizer.State.Oscillator as OsciS

import System.Exit (ExitCode, )
import NumericPrelude
import Prelude ()

playState :: Sig.T Double -> IO ExitCode
playState =
   Play.simple SigSt.hPut SoxOpt.none 44100 .
   SigG.fromState SigG.defaultLazySize .
   Sig.map BinSmp.int16FromDouble

play :: SigSt.T Double -> IO ExitCode
play =
   Play.simple SigSt.hPut SoxOpt.none 44100 .
   SigSt.map BinSmp.int16FromDouble

mSine = Osci.static SigG.defaultLazySize Wave.sine zero

mSaw = Osci.static SigG.defaultLazySize Wave.saw zero

mSaw2 = OsciS.static Wave.saw zero

mOscillator1 :: IO ExitCode
mOscillator1 =
   play $ mOsc2 (mSaw (0.00008::Double)) (mSine (0.0005::Double)) (mSine (0.02::Double))

mOscillator2 :: IO ExitCode
mOscillator2 =
   play $ mOsc2 (mSine (0.0008::Double)) (mSaw (0.005::Double)) (mSine (0.04::Double))

mOscillator3 :: IO ExitCode
mOscillator3 = do
  playState $ mOsc3 (CtrlS.exponential2 50000 1) (mSaw2 (0.02::Double))
  playState $ mOsc3 (CtrlS.exponential2 50000 1) (mSaw2 (0.08::Double))

poleFilter pole =
   SigG2.modifyModulated UniFilter.modifier (SigG2.map (\f -> UniFilter.parameter $ FiltRec.Pole 10 (0.04+0.02*f)) $ pole)

mOsc2 env pole snd =
   Filt.envelope env $
     SigG2.map UniFilter.lowpass $
     poleFilter pole $
     snd

mOsc3 :: Sig.T Double -> Sig.T Double -> Sig.T Double
mOsc3 env snd =
   Filt.envelope env $ Sig.map UniFilter.lowpass $ Sig.modifyModulated UniFilter.modifier (Sig.map (\f -> UniFilter.parameter $ FiltRec.Pole 10 (0.03*f)) $ env) $ snd
