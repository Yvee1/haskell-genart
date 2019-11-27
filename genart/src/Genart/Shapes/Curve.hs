{-# LANGUAGE FlexibleInstances #-}
module Genart.Shapes.Curve where

import Genart.CairoHelpers
import Genart.Shapes.Types

segments :: Curve -> [Line]
segments (Curve (pt1 : pt2 : pts)) = Line pt1 pt2 : segments (Curve (pt2 : pts))
segments _ = []