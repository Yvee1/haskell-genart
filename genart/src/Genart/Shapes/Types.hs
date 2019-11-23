{-# LANGUAGE FlexibleInstances #-}

module Genart.Shapes.Types (
  module Genart.Shapes.Types,
  module Linear.Affine,
  module Linear.Metric
) where

import Genart.CairoHelpers
import Genart.Random
import Linear.Affine
import Linear.Metric

import Data.Random
import Data.RVar

-------------------------------
-- Type classes

class Draw a where
  draw :: a -> Render ()

class Shape s where
  randomInside :: s -> Generate Pt

class Trail t where
  pointsOn :: t -> [Pt]

class PtLike p where
  getX :: p -> Double
  getY :: p -> Double

instance PtLike (Double, Double) where
  getX (x, _) = x
  getY (_, y) = y

-- instance PtLike (Integer, Integer) where
--   getX (x, _) = fromIntegral x
--   getY (_, y) = fromIntegral y

class Smooth a where
  chaikinStep :: a -> a

  chaikin :: a -> a
  chaikin a = iterate chaikinStep a !! 5

class PtList t where
  (#) :: ([Pt] -> [Pt]) -> t -> t
  (##) :: ([Pt] -> a) -> t -> a
  (%%) :: (t -> a) -> [Pt] -> a

-------------------------------
-- Point

type Pt = Point V2 Double

instance Draw Pt where
  draw (P (V2 x y)) = arc x y 1 0 (2 * pi)

instance Trail Pt where
  pointsOn p = [p]

instance PtLike Pt where
  getX (P (V2 x _)) = x
  getY (P (V2 _ y)) = y

point :: Double -> Double -> Pt
point x y = P (V2 x y)

infix 5 .&
(.&) :: Double -> Double -> Pt
(.&) = point

randomPt :: (MonadRandom m) => (Double, Double) -> (Double, Double) -> m Pt
randomPt xrange yrange =
  do vec <- randomVec xrange yrange
     return (P vec)

-------------------------------
-- Vector

type Vec = V2 Double

instance Draw Vec where
  draw v = polyline [P (V2 0 0), P v]

instance PtLike Vec where
  getX (V2 x _) = x
  getY (V2 _ y) = y

infix 5 ^&
(^&) :: Double -> Double -> Vec
(^&) = V2

polyline :: [Pt] -> Render ()
polyline (P (V2 x y) : ps) = do
  newPath
  moveTo x y
  for_ ps $ \(P (V2 x' y')) -> lineTo x' y'

randomVec :: (MonadRandom m) => (Double, Double) -> (Double, Double) -> m Vec
randomVec (x1, x2) (y1, y2) = 
  do x <- x1 <=> x2
     y <- y1 <=> y2
     pure $ V2 x y

-------------------------------
-- Polygon

newtype Polygon = Polygon [Pt]
  deriving (Eq, Show)

instance Draw Polygon where
  draw (Polygon pts) = renderClosedPath pts

instance Trail Polygon where
  pointsOn (Polygon pts) = closed pts

instance Smooth Polygon where
  chaikinStep (Polygon pts) = Polygon (generalChaikinStep $ closed pts)

instance Shape Polygon where
  randomInside p@(Polygon pts)
    | length pts == 3 = randomInsideTriangle p
    | otherwise = undefined

instance PtList Polygon where
  f # Polygon pts = Polygon (f pts)
  f ## Polygon pts = f pts
  (%%) = (. Polygon)
  -- f %% pts = (\(Polygon pts) -> pts) (f poly)

randomInsideTriangle :: Polygon -> Generate Pt
randomInsideTriangle (Polygon [pt1, pt2, pt3]) = 
  -- let v1 = pt2 .-. pt1
  --     v2 = pt3 .-. pt1
  let P v1 = pt1
      P v2 = pt2
      P v3 = pt3 
      in
        do r1 <- random
           r2 <- random
          --  let x = pt1 .+^ (r1 *^ v1 + r2 *^ v2)
           let x = (1 - sqrt r1) *^ v1 + (sqrt r1 * (1 - r2)) *^ v2 + (r2 * sqrt r1) *^ v3
           pure (P x)

closed :: [Pt] -> [Pt]
closed pts = pts ++ [head pts]

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

instance Trail Line where
  pointsOn (Line pt1 pt2) = [pt1, pt2] 

-------------------------------
-- Curve

newtype Curve = Curve [Pt]
  deriving (Show, Eq)

instance Draw Curve where
  draw (Curve pts) = polyline pts

instance Trail Curve where
  pointsOn (Curve pts) = pts

instance Smooth Curve where
  chaikinStep (Curve pts) = Curve (generalChaikinStep pts)

instance PtList Curve where
  f # Curve pts = Curve (f pts)
  f ## Curve pts = f pts

generalChaikinStep :: [Pt] -> [Pt]
generalChaikinStep (pt1 : pt2 : pts) = [pt1 * 0.75 + pt2 * 0.25, pt1 * 0.25 + pt2 * 0.75] ++ generalChaikinStep (pt2 : pts)
generalChaikinStep _ = []

infixl 4 ~~
(~~) :: (Trail t1, Trail t2) => t1 -> t2 -> Curve
t1 ~~ t2 = Curve (pointsOn t1 ++ pointsOn t2) 

-------------------------------
-- Ellipse
-- Two foci and the constant
-- data Ellipse = Ellipse Pt Pt Double

