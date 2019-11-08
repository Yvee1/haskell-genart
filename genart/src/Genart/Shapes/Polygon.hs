{-# LANGUAGE FlexibleInstances #-}
module Genart.Shapes.Polygon where

import Genart.CairoHelpers
import Genart.Shapes.Types

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

gngon :: Int -> [Pt] -> Int -> Pt -> Double -> Polygon
gngon 0 pts n c r = Polygon pts
gngon i pts n c@(P (V2 x y)) r = gngon (i-1) (pt : pts) n c r
  where angle = 2 * pi * fromIntegral i / fromIntegral n - pi / 2
        pt = point (x + r * cos angle) (y + r * sin angle)

ngon:: Int -> Pt -> Double -> Polygon
ngon sides center radius = gngon sides [] sides center radius

triangle :: Pt -> Double -> Polygon
triangle = ngon 3

square :: Pt -> Double -> Polygon
square = ngon 4

pentagon :: Pt -> Double -> Polygon
pentagon = ngon 5

hexagon :: Pt -> Double -> Polygon
hexagon = ngon 6

heptagon :: Pt -> Double -> Polygon
heptagon = ngon 7

octagon :: Pt -> Double -> Polygon
octagon = ngon 8

nonagon :: Pt -> Double -> Polygon
nonagon = ngon 9

decagon :: Pt -> Double -> Polygon
decagon = ngon 10