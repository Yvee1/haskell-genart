{-# LANGUAGE FlexibleInstances #-}
module Genart.Shapes.Circle where

import Genart.CairoHelpers
import Genart.Shapes.Types

circle :: PtLike p => p -> Double -> Circle
circle = Circle . getPt