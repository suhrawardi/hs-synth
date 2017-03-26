module Main where

import Plain
import Realtime
import UI
import FRP.UISF

main = filterSaw

-- main :: IO ()
-- main = runUI (defaultUIParams {uiSize=(200, 520), uiCloseOnEsc=True}) $
--   (leftRight $ (bottomUp $ timeEx)) >>> freq
