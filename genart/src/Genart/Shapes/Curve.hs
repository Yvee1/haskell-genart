{-# LANGUAGE FlexibleInstances #-}
module Genart.Shapes.Curve where

import Genart.CairoHelpers
import Genart.Shapes.Types

segments :: Curve -> [Line]
segments (Curve (pt1 : pt2 : pts)) = Line pt1 pt2 : segments (Curve (pt2 : pts))
segments _ = []

arcCurve :: Double -> Pt -> Double -> Double -> Double -> Curve
arcCurve n pt r aStart aEnd = Curve pts
  where pts = map makePt [0..n] 
        makePt t = let a = aStart + t / n * (aEnd - aStart) in
          pt + ((r * cos a) :& (r * sin a))