module Main where

import Plain
import Realtime
import UI
import FRP.UISF
import MineTwo
import Mine

-- main = mLaser
main = playStereo stereoSignal
-- main = mOscillator3

playM f = playStereo $ f (44100::Double)

-- main :: IO ()
-- main = runUI (defaultUIParams {uiSize=(200, 520), uiCloseOnEsc=True}) $
--   (leftRight $ (bottomUp $ timeEx)) >>> freq
