module Main where

import Plain
import Realtime
import UI
import FRP.UISF

-- main = rFilterPingState

main :: IO ()
main = runUI (defaultUIParams {uiSize=(200, 520), uiCloseOnEsc=True}) $
  (leftRight $ (bottomUp $ timeEx)) >>> freq
