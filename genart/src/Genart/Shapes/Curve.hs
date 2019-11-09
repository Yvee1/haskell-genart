{-# LANGUAGE FlexibleInstances #-}
module Genart.Shapes.Curve where

import Genart.CairoHelpers
import Genart.Shapes.Types

chaikinCurveStep :: Curve -> Curve
chaikinCurveStep (Curve pts) = Curve (chaikinStep pts)