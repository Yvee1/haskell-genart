{-# LANGUAGE FlexibleInstances #-}
module Genart.Shapes.Line where

import Genart.CairoHelpers
import Genart.Shapes.Types

intersectRay :: Line -> Line -> Maybe Pt
intersectRay (Line (P (V2 x1 y1)) (P (V2 x2 y2))) (Line (P (V2 x3 y3)) (P (V2 x4 y4))) =
  let denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4) in
    if denominator == 0 then Nothing else
      let a = (x1 * y2 - y1 * x2)
          b = (x3 * y4 - y3 * x4) in
            Just $ P $ V2 (((x3 - x4) * a - (x1 - x2) * b) / denominator)
                          (((y3 - y4) * a - (y1 - y2) * b) / denominator)

-- infix 4 .~
-- (.~) :: Pt -> Vec -> Line
-- pt .~ v = Line pt (pt .+^ v)