{-# LANGUAGE FlexibleInstances #-}
module Genart.Shapes.Curve where

import Genart.CairoHelpers
import Genart.Shapes.Types

infixl 4 ~~
(~~) :: (Trail t1, Trail t2) => t1 -> t2 -> Curve
t1 ~~ t2 = Curve (pointsOn t1 ++ pointsOn t2) 