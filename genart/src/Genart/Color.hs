module Genart.Color where

import Genart.CairoHelpers

black :: Double -> Render ()
black = hsva 0 0 0

white :: Double -> Render ()
white = hsva 0 0 1

grey :: Double -> Double -> Render ()
grey = hsva 0 0