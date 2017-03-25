module Plain (sineMono, sineStereo, oscillator, saw, cubic,
              sawMorph, laser, ping, fmPing, filterSaw) where

import qualified Synthesizer.Plain.Play as Play
import qualified Synthesizer.Plain.File as File
import qualified Synthesizer.Plain.Signal as Sig
import qualified Synthesizer.Plain.Control as Ctrl
import qualified Synthesizer.Plain.Oscillator as Osci
import qualified Synthesizer.Plain.Filter.NonRecursive as Filt
import qualified Synthesizer.Plain.Filter.Recursive as FiltRec
import qualified Synthesizer.Plain.Filter.Recursive.Universal as UniFilter
import qualified Synthesizer.Basic.Wave as Wave

import qualified Algebra.Module as Module -- needed for Haddock

import System.Exit (ExitCode, )
import NumericPrelude
import Prelude ()

playMono :: Sig.T Double -> IO ExitCode
playMono = Play.monoToInt16 (44100::Double)

playStereo :: [(Double, Double)] -> IO ExitCode
playStereo = Play.stereoToInt16 (44100::Double)

sineMono :: IO ExitCode
sineMono =
   playMono (map sin [0::Double,0.1..])

sineStereo :: IO ExitCode
sineStereo = do
   playStereo $ zip (map sin [0::Double, 0.0998..]) (map sin [0::Double, 0.1002..])
   playStereo $ zip (map sin [0::Double, 0.1004..]) (map sin [0::Double, 0.0996..])
   playStereo $ zip (map sin [0::Double, 0.0994..]) (map sin [0::Double, 0.1006..])
   playStereo $ zip (map sin [0::Double, 0.1008..]) (map sin [0::Double, 0.0992..])

oscillator :: IO ExitCode
oscillator =
   playStereo $ zip (Osci.static Wave.sine 0 (0.01::Double))
                    (Osci.static Wave.sine 0 (0.01001::Double))

saw :: IO ExitCode
saw =
   playStereo $ zip (Osci.static (Wave.triangleAsymmetric 0.9) 0 (0.01::Double))
                    (Osci.static (Wave.triangleAsymmetric 0.9) 0 (0.0102::Double))

cubic :: IO ExitCode
cubic =
   playStereo $ zip (Osci.static (Wave.distort (^3) Wave.saw) 0 (0.01::Double))
                    (Osci.static (Wave.distort (^3) Wave.saw) 0 (0.0102::Double))

sawMorph :: IO ExitCode
sawMorph =
   playStereo $ zip (Osci.shapeMod Wave.triangleAsymmetric 0 (0.01::Double) (Osci.static Wave.sine 0 (0.00001::Double)))
                    (Osci.shapeMod Wave.triangleAsymmetric 0 (0.01::Double) (Osci.static Wave.sine 0 (0.00002::Double)))

laser :: IO ExitCode
laser =
   playMono (Osci.freqMod Wave.saw 0 $ map (\f -> 0.02+0.01*f) $ Osci.static Wave.saw 0 (0.0001::Double))

pingSig :: Sig.T Double
pingSig =
   Filt.envelope (Ctrl.exponential 50000 1) (Osci.static Wave.sine 0 (0.01::Double))

ping :: IO ExitCode
ping = playMono pingSig

fmPing :: IO ExitCode
fmPing =
   playMono (Osci.phaseMod Wave.sine (0.01::Double) $ map (2*) pingSig)

filterSaw :: IO ExitCode
filterSaw =
   playMono (map UniFilter.lowpass $ UniFilter.run (map (\f -> UniFilter.parameter $ FiltRec.Pole 10 (0.04+0.02*f)) $ Osci.static Wave.sine 0 (0.00001::Double)) $ Osci.static Wave.saw 0 (0.002::Double))
