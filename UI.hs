{-# LANGUAGE Arrows #-}
module UI (freq, timeEx) where

import Plain
import FRP.UISF
import FRP.UISF.Graphics

import Numeric (showHex)

timeEx :: UISF () ()
timeEx = title "time" $ accumTime >>> display <<< spacer

freq :: UISF () ()
freq = title "Freq" $ topDown $ proc _ -> do
  a <- spacer <<< title "all"  (hiSlider 1 (0,255) 128) -< ()
  title "freq" display -< a
