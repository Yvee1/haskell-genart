{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

-- class Draw

class Shape s where
  randomInside :: s -> Generate Pt
  boundingBox :: s -> Polygon

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

infix 5 :&
pattern (:&) x y = P (V2 x y) :: Pt

instance Draw Pt where
  draw (x :& y) = arc x y 0.5 0 (2 * pi)

instance Trail Pt where
  pointsOn p = [p]

instance PtLike Pt where
  getX (x :& _) = x
  getY (_ :& y) = y

point :: Double -> Double -> Pt
point x y = x :& y

-- infix 5 .&
-- (.&) :: Double -> Double -> Pt
-- (.&) = point

randomPt :: (MonadRandom m) => (Double, Double) -> (Double, Double) -> m Pt
randomPt xrange yrange =
  do vec <- randomVec xrange yrange
     return (P vec)

-------------------------------
-- Vector

type Vec = V2 Double

instance Draw Vec where
  draw v = polyline [0 :& 0, P v]

instance PtLike Vec where
  getX (V2 x _) = x
  getY (V2 _ y) = y

infix 5 ^&
(^&) :: Double -> Double -> Vec
(^&) = V2

polyline :: [Pt] -> Render ()
polyline ((x :& y) : ps) = do
  newPath
  moveTo x y
  for_ ps $ \(x' :& y') -> lineTo x' y'

randomVec :: (MonadRandom m) => (Double, Double) -> (Double, Double) -> m Vec
randomVec (x1, x2) (y1, y2) = 
  do x <- x1 <=> x2
     y <- y1 <=> y2
     pure $ V2 x y

-------------------------------
-- Grid

newtype Grid a = Grid [[(Pt, a)]]
  deriving Show

instance Trail (Grid a) where
  pointsOn (Grid g) = concatMap (map fst) g

instance Draw (Grid a) where
  draw = drawPoints . pointsOn
  
drawPoints :: [Pt] -> Render ()
drawPoints pts = for_ pts $ \pt@(x :& y) -> moveTo (x+0.5) y *> draw pt

valuesOn :: Grid a -> [a]
valuesOn (Grid g) = concatMap (map snd) g

row :: Int -> Grid a -> [(Pt, a)]
row n (Grid g) = g !! n

column :: Int -> Grid a -> [(Pt, a)]
column n (Grid g) = map (!! n) g

makeGrid :: [Double] -> [Double] -> (Double -> Double -> a) -> Grid a
makeGrid xs ys f = Grid g
  where g = [[(x :& y, f x y) | x <- xs] | y <- ys]

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
  randomInside p
    | length ## p == 3 = randomInsideTriangle p
    | otherwise = undefined
  
  boundingBox = (boundingBoxPts ##)
  
boundingBoxPts :: [Pt] -> Polygon
boundingBoxPts ((x1 :& y1) : pts) = boundingBoxPts' x1 x1 y1 y1 pts
  where
    boundingBoxPts' l r t b [] = Polygon [l :& b, l :& t, r :& t, r :& b]
    boundingBoxPts' l r t b ((x :& y) : pts) = boundingBoxPts' (l `min` x) (r `max` x) (t `max` y) (b `min` y) pts

instance PtList Polygon where
  f # Polygon pts = Polygon (f pts)
  f ## Polygon pts = f pts
  (%%) = (. Polygon)

randomInsideTriangle :: Polygon -> Generate Pt
randomInsideTriangle (Polygon [pt1, pt2, pt3]) = 
  let P v1 = pt1
      P v2 = pt2
      P v3 = pt3 
      in
        do r1 <- random
           r2 <- random
           let x = (1 - sqrt r1) *^ v1 + (sqrt r1 * (1 - r2)) *^ v2 + (r2 * sqrt r1) *^ v3
           return (P x)

closed :: [Pt] -> [Pt]
closed pts = pts ++ [head pts]

renderClosedPath :: [Pt] -> Render ()
renderClosedPath [] = pure ()
renderClosedPath ((x :& y) : pts) = do
  newPath
  moveTo x y
  for_ pts $ \(x' :& y') -> lineTo x' y'
  closePath

infix 5 @@
(@@) :: Polygon -> Int -> Pt
(Polygon pts) @@ n = pts !! n

square :: PtLike p => p -> Double -> Polygon
square p w = square' p (w/2)
  where p = point (getX p) (getY p)

square' :: Pt-> Double -> Polygon
square' p r = rect' p r r

rect :: PtLike p => p -> Double -> Double -> Polygon
rect p wx wy = rect' p (wx/2) (wy/2)
  where p = point (getX p) (getY p)

rect' :: Pt -> Double -> Double -> Polygon
rect' p rx ry = Polygon [p .+^ V2 (-rx) (-ry), p .+^ V2 (-rx) ry, p .+^ V2 rx ry, p .+^ V2 rx (-ry)]

-------------------------------
-- Circle

data Circle = Circle Pt Double
  deriving (Show, Eq)

instance Draw Circle where
  draw (Circle (x :& y) r) = arc x y r 0 (2 * pi)

instance Shape Circle where
  -- Concentrated at center, can also be useful though

  -- randomInside (Circle (cx :& cy) radius) = do
  --   r <- 0 <=> radius
  --   a <- 0 <=> (2 * pi)
  --   return $ (cx + r * cos a) :& (cy + r * sin a)

  -- uniform
  randomInside (Circle (cx :& cy) radius) = do
    r' <- random
    let r = radius * sqrt r'
    a <- 0 <=> (2 * pi)
    return $ (cx + r * cos a) :& (cy + r * sin a)

  boundingBox (Circle center radius) =
    square' center radius

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
  (%%) = (. Curve)

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

