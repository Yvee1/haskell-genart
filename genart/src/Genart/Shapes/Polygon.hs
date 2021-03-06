{-# LANGUAGE FlexibleInstances, TypeApplications #-}
module Genart.Shapes.Polygon where

import Genart.CairoHelpers
import Genart.Shapes.Types
import Genart.Shapes.Line (intersectRay)
import Data.Maybe (fromMaybe)

getSpace :: Generate Polygon
getSpace = do
  (w, h) <- getSize @Double
  return $ Polygon [0:&0, w:&0, w:&h, 0:&h]

polygon :: [Pt] -> Polygon
polygon = Polygon

gngon :: PtLike p => Int -> [Pt] -> Int -> p -> Double -> Polygon
gngon 0 pts n c r = Polygon pts
gngon i pts n c r = gngon (i-1) (pt : pts) n c r
  where angle = 2 * pi * fromIntegral (n-i+1) / fromIntegral n - pi / 2
        pt = point (x + r * cos angle) (y + r * sin angle)
        x = getX c
        y = getY c

ngon:: PtLike p => Int -> p -> Double -> Polygon
ngon sides center radius = gngon sides [] sides center radius

triangle :: PtLike p => p -> Double -> Polygon
triangle = ngon 3

tetragon :: PtLike p => p -> Double -> Polygon
tetragon = ngon 4

pentagon :: PtLike p => p -> Double -> Polygon
pentagon = ngon 5

hexagon :: PtLike p => p -> Double -> Polygon
hexagon = ngon 6

heptagon :: PtLike p => p -> Double -> Polygon
heptagon = ngon 7

octagon :: PtLike p => p -> Double -> Polygon
octagon = ngon 8

nonagon :: PtLike p => p -> Double -> Polygon
nonagon = ngon 9

decagon :: PtLike p => p -> Double -> Polygon
decagon = ngon 10

adjacentSides :: Polygon -> Int -> (Line, Line)
adjacentSides (Polygon pts) i = 
  (Line (pts !! i) (pts !! ((i+1) `rem` n)),
   Line (pts !! i) (pts !! ((i-1) `mod` n)))
    where n = length pts

area :: Polygon -> Double
area (Polygon pts) = abs $ area' 0 (closed pts)
  where area' a (p1 : p2 : ps) = area' (a + det p1 p2) (p2 : ps)
        area' a _ = a
        det (P (V2 x1 y1)) (P (V2 x2 y2)) = x1 * y2 - y1 * x2 

perimeter :: Polygon -> Double
perimeter (Polygon pts) = perimeter' 0 (closed pts)
  where perimeter' :: Double -> [Pt] -> Double
        perimeter' p (p1 : p2 : ps) = perimeter' (p + norm (p1 .-. p2)) (p2 : ps)
        perimeter' p _ = p
        
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
inradius poly = area poly / perimeter poly

incircle :: Polygon -> Circle
incircle polygon = Circle (incenter polygon) (inradius polygon)

triangulateConvexPoints :: [Pt] -> [[Pt]]
triangulateConvexPoints pts
  | length pts < 3 = []
  | otherwise = let (pt1 : pt2 : pt3 : stuff) = pts in
    [pt1, pt2, pt3] : triangulateConvexPoints (pt1 : pt3 : stuff) 

triangulateConvexPolygon :: Polygon -> [Polygon]
triangulateConvexPolygon (Polygon pts) = map Polygon $ triangulateConvexPoints pts

triangulateConvexPointsFromCenter :: [Pt] -> [[Pt]]
triangulateConvexPointsFromCenter pts = triangulateConvexPointsFromCenter' (centroid %% pts) (closed pts) where
  triangulateConvexPointsFromCenter' c points
    | length points < 2 = []
    | otherwise = let (pt1 : pt2 : stuff) = points in
      [c, pt1, pt2] : triangulateConvexPointsFromCenter' c (pt2 : stuff) 

triangulateConvexPolygonFromCenter :: Polygon -> [Polygon]
triangulateConvexPolygonFromCenter poly = (map Polygon . triangulateConvexPointsFromCenter) ## poly

centroid :: Polygon -> Pt
centroid (Polygon pts) = sum pts / fromIntegral (length pts)