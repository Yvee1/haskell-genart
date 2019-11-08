{-# LANGUAGE FlexibleInstances #-}

module Genart.Shapes (
  module Genart.Shapes,
  module Linear.Affine
) where

import Genart.CairoHelpers
import Linear.Affine

class Draw a where
  draw :: a -> Render ()

type Vec = V2 Double
type Pt = Point V2 Double

instance Draw Pt where
  draw (P (V2 x y)) = arc x y 1 0 (2 * pi)

instance Draw Vec where
  draw v = line [V2 0 0, v]

newtype Polygon = Polygon [Pt]
  deriving (Eq)
instance Draw Polygon where
  draw (Polygon pts) = renderClosedPath pts

renderClosedPath :: [Pt] -> Render ()
renderClosedPath [] = pure ()
renderClosedPath ((P (V2 x y)) : pts) = do
  newPath
  moveTo x y
  for_ pts $ \(P (V2 x' y')) -> lineTo x' y'
  closePath

line :: [V2 Double] -> Render ()
line (V2 x y: vs) = do
  newPath
  moveTo x y
  for_ vs $ \(V2 x' y') -> lineTo x' y'