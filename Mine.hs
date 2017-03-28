{-# LANGUAGE FlexibleContexts #-}
module Mine (mOscillator1, mOscillator2)
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

play :: SigSt.T Double -> IO ExitCode
play =
   Play.simple SigSt.hPut SoxOpt.none 44100 .
   SigSt.map BinSmp.int16FromDouble

mSine = Osci.static SigG.defaultLazySize Wave.sine zero

mSaw = Osci.static SigG.defaultLazySize Wave.saw zero

mOscillator1 :: IO ExitCode
mOscillator1 =
   play $ mOsc2 (mSaw (0.00008::Double)) (mSine (0.0005::Double)) (mSine (0.02::Double))

mOscillator2 :: IO ExitCode
mOscillator2 =
   play $ mOsc2 (mSine (0.0008::Double)) (mSaw (0.005::Double)) (mSine (0.04::Double))

poleFilter pole =
   SigG2.modifyModulated UniFilter.modifier (SigG2.map (\f -> UniFilter.parameter $ FiltRec.Pole 10 (0.04+0.02*f)) $ pole)
   -- SigG2.modifyModulated UniFilter.modifier (SigG2.map (\f -> UniFilter.parameter $ mod) $ pole)

mOsc2 env pole snd =
   Filt.envelope env $
     SigG2.map UniFilter.lowpass $
     poleFilter pole $
     snd

oscSig ::
   (SigG.Write sig Double,
    SigG2.Transform sig (UniFilter.Result Double) Double,
    SigG2.Transform sig Double (UniFilter.Result Double),
    SigG2.Transform sig Double (UniFilter.Parameter Double)) =>
   sig Double
oscSig =
   SigG2.map UniFilter.lowpass $ SigG2.modifyModulated UniFilter.modifier (SigG2.map (\f -> UniFilter.parameter $ FiltRec.Pole 10 (0.04+0.02*f)) $ Osci.static SigG.defaultLazySize Wave.sine zero (0.00001::Double)) $ Osci.static SigG.defaultLazySize Wave.sine zero (0.002::Double)

mOscSig :: IO ExitCode
mOscSig = play oscSig
