module Genart.ChaosboxWrapper 
  ( module C
  , onMouseMotion
  ) where

import Genart.Shapes.Types
import Genart.CairoHelpers
import ChaosBox.Interactive  as C hiding (onMouseMotion)
import qualified ChaosBox.Interactive as CI

onMouseMotion :: (Pt -> Generate ()) -> Generate ()
onMouseMotion f = CI.onMouseMotion (f . getPt)