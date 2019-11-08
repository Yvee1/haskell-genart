{-# LANGUAGE FlexibleInstances #-}
module Genart.Shapes.Types (
  module Genart.Shapes.Types,
  module Linear.Affine
) where

import Genart.CairoHelpers
import Linear.Affine

type Vec = V2 Double
type Pt = Point V2 Double

class Draw a where
  draw :: a -> Render ()

-- class Shape s where
  -- center :: s -> Pt

instance Draw Pt where
  draw (P (V2 x y)) = arc x y 1 0 (2 * pi)

instance Draw Vec where
  draw v = line [V2 0 0, v]

line :: [V2 Double] -> Render ()
line (V2 x y: vs) = do
  newPath
  moveTo x y
  for_ vs $ \(V2 x' y') -> lineTo x' y'

point :: Double -> Double -> Pt
point x y = P (V2 x y)

infix 5 .&
(.&) :: Double -> Double -> Pt
x .& y = P (V2 x y)

infix 5 ^&
(^&) :: Double -> Double -> Vec
x ^& y = V2 x y