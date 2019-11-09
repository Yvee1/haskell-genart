{-# LANGUAGE FlexibleInstances #-}
module Genart.Shapes.Types (
  module Genart.Shapes.Types,
  module Linear.Affine,
  module Linear.Metric
) where

import Genart.CairoHelpers
import Linear.Affine
import Linear.Metric

type Vec = V2 Double
type Pt = Point V2 Double

class Draw a where
  draw :: a -> Render ()

-- class Shape s where
  -- center :: s -> Pt

instance Draw Pt where
  draw (P (V2 x y)) = arc x y 1 0 (2 * pi)

instance Draw Vec where
  draw v = polyline [P (V2 0 0), P v]

polyline :: [Pt] -> Render ()
polyline (P (V2 x y) : ps) = do
  newPath
  moveTo x y
  for_ ps $ \(P (V2 x' y')) -> lineTo x' y'

point :: Double -> Double -> Pt
point x y = P (V2 x y)

infix 5 .&
(.&) :: Double -> Double -> Pt
x .& y = P (V2 x y)

infix 5 ^&
(^&) :: Double -> Double -> Vec
x ^& y = V2 x y

-------------------------------
-- Polygon

newtype Polygon = Polygon [Pt]
  deriving (Eq, Show)
instance Draw Polygon where
  draw (Polygon pts) = renderClosedPath pts

renderClosedPath :: [Pt] -> Render ()
renderClosedPath [] = pure ()
renderClosedPath (P (V2 x y) : pts) = do
  newPath
  moveTo x y
  for_ pts $ \(P (V2 x' y')) -> lineTo x' y'
  closePath

infix 5 @@
(@@) :: Polygon -> Int -> Pt
(Polygon pts) @@ n = pts !! n

-------------------------------
-- Circle

data Circle = Circle Pt Double
  deriving (Show, Eq)

instance Draw Circle where
  draw (Circle (P (V2 x y)) r) = arc x y r 0 (2 * pi)

-------------------------------
-- Line

data Line = Line Pt Pt
  deriving (Show, Eq)

instance Draw Line where
  draw (Line pt1 pt2) = polyline [pt1, pt2] 