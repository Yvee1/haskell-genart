{-# LANGUAGE FlexibleInstances #-}
module Genart.Shapes.Polygon where

import Genart.CairoHelpers
import Genart.Shapes.Types
import Genart.Shapes.Line (intersectRay)
import Data.Maybe (fromMaybe)

polygon :: [Pt] -> Polygon
polygon = Polygon

gngon :: Int -> [Pt] -> Int -> Pt -> Double -> Polygon
gngon 0 pts n c r = Polygon pts
gngon i pts n c@(P (V2 x y)) r = gngon (i-1) (pt : pts) n c r
  where angle = 2 * pi * fromIntegral (n-i+1) / fromIntegral n - pi / 2
        pt = point (x + r * cos angle) (y + r * sin angle)

ngon:: Int -> Pt -> Double -> Polygon
ngon sides center radius = gngon sides [] sides center radius

triangle :: Pt -> Double -> Polygon
triangle = ngon 3

tetragon :: Pt -> Double -> Polygon
tetragon = ngon 4

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

square :: Pt -> Double -> Polygon
square p w = square' p (w/2)

square' :: Pt -> Double -> Polygon
square' p r = rect' p r r

rect :: Pt -> Double -> Double -> Polygon
rect p wx wy = rect' p (wx/2) (wy/2)

rect' :: Pt -> Double -> Double -> Polygon
rect' p rx ry = Polygon [p .+^ V2 (-rx) (-ry), p .+^ V2 (-rx) ry, p .+^ V2 rx ry, p .+^ V2 rx (-ry)]

adjacentSides :: Polygon -> Int -> (Line, Line)
adjacentSides (Polygon pts) i = 
  (Line (pts !! i) (pts !! ((i+1) `rem` n)),
   Line (pts !! i) (pts !! ((i-1) `mod` n)))
    where n = length pts

bisector :: Polygon -> Int -> Vec
bisector p i = 
  let (Line a1 a2, Line b1 b2) = adjacentSides p i in
    (normalize (a2 .-. a1) ^+^ normalize (b2 .-. b1)) / 2

incenter :: Polygon -> Pt
incenter p@ (Polygon (pt1 : pt2 : _)) =
  let a = pt1 .+^ bisector p 0
      b = pt2 .+^ bisector p 1
      in
        fromMaybe pt1 $ intersectRay (Line pt1 a) (Line pt2 b)

inradius :: Polygon -> Double
inradius (Polygon (pt1 : pt2 : pts)) = 
  let len = norm (pt1 .-. pt2) 
      n = length (pt1 : pt2 : pts)
        in
          len / (2 * tan (pi / fromIntegral n))

incircle :: Polygon -> Circle
incircle polygon = Circle (incenter polygon) (inradius polygon)