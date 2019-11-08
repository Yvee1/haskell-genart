{-# LANGUAGE FlexibleInstances #-}
module Genart.Shapes.Circle where

import Genart.CairoHelpers
import Genart.Shapes.Types

data Circle = Circle Pt Double
  deriving (Show, Eq)

instance Draw Circle where
  draw (Circle (P (V2 x y)) r) = arc x y r 0 (2 * pi)

circle :: Pt -> Double -> Circle
circle = Circle