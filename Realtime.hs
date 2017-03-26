{-# LANGUAGE FlexibleContexts #-}
module Realtime (play, rOscillator, rFilterSaw, rFilterSawState,
                 rFilterPingState, rFilterPingCausal)
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

rOscillator :: IO ExitCode
rOscillator =
   play (Osci.static SigG.defaultLazySize Wave.sine zero (0.01::Double))

filterSawSig ::
   (SigG.Write sig Double,
    SigG2.Transform sig (UniFilter.Result Double) Double,
    SigG2.Transform sig Double (UniFilter.Result Double),
    SigG2.Transform sig Double (UniFilter.Parameter Double)) =>
   sig Double
filterSawSig =
   SigG2.map UniFilter.lowpass $ SigG2.modifyModulated UniFilter.modifier (SigG2.map (\f -> UniFilter.parameter $ FiltRec.Pole 10 (0.04+0.02*f)) $ Osci.static SigG.defaultLazySize Wave.sine zero (0.00001::Double)) $ Osci.static SigG.defaultLazySize Wave.saw zero (0.002::Double)

rFilterSaw :: IO ExitCode
rFilterSaw =
   play filterSawSig

playState :: Sig.T Double -> IO ExitCode
playState =
   Play.simple SigSt.hPut SoxOpt.none 44100 .
   SigG.fromState SigG.defaultLazySize .
   Sig.map BinSmp.int16FromDouble

rFilterSawState :: IO ExitCode
rFilterSawState =
   playState filterSawSig

filterPingStateProc :: Sig.T Double -> Sig.T Double
filterPingStateProc env =
   Filt.envelope env $ Sig.map UniFilter.lowpass $ Sig.modifyModulated UniFilter.modifier (Sig.map (\f -> UniFilter.parameter $ FiltRec.Pole 10 (0.03*f)) $ env) $ OsciS.static Wave.saw zero (0.002::Double)

rFilterPingState :: IO ExitCode
rFilterPingState =
   playState $
   filterPingStateProc $
   CtrlS.exponential2 50000 1

filterPingShare :: IO ExitCode
filterPingShare =
   playState $
   filterPingStateProc $
   Sig.fromList $ Sig.toList $ CtrlS.exponential2 50000 1

rFilterPingCausal :: IO ExitCode
rFilterPingCausal =
   playState $
   let proc =
          uncurry (*) ^<<
          ((UniFilter.lowpass ^<<
            UniFilter.causal <<<
            Causal.feedSnd (OsciS.static Wave.saw zero (0.002::Double)) <<^
            (\f -> UniFilter.parameter $ FiltRec.Pole 10 (0.03*f)))
           &&&
           Causal.id)
   in  Causal.apply proc $ CtrlS.exponential2 50000 1
