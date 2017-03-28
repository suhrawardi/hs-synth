module Main where

import Plain
import Realtime
import UI
import FRP.UISF
import FilterSaw
import ChildSong
import Mine

-- main = mLaser
-- main = playM reverbedSignal
main = mOscillator3

playM f = playStereo $ f  (44100::Double)

-- main :: IO ()
-- main = runUI (defaultUIParams {uiSize=(200, 520), uiCloseOnEsc=True}) $
--   (leftRight $ (bottomUp $ timeEx)) >>> freq
