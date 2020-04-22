{-# LANGUAGE FlexibleInstances, PatternSynonyms, ScopedTypeVariables, DeriveFoldable, DeriveFunctor, DeriveTraversable, DerivingStrategies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Genart.Shapes.Types (
  module Genart.Shapes.Types,
  module Linear.Affine,
  module Linear.Metric
) where

import Genart.CairoHelpers hiding (x, y)
import Genart.Random
import Linear.Affine
import Linear.Metric

import Data.Random
import Data.RVar

import ChaosBox (Draw, draw)

-------------------------------
-- Type classes

class Trail t where
  pointsOn :: t -> [Pt]

class PtLike p where
  {-# MINIMAL getX, getY, setX, setY #-}

  getX :: p -> Double
  getY :: p -> Double
  setX :: p -> Double -> p
  setY :: p -> Double -> p

  getPt :: p -> Pt
  getPt ptLike = getX ptLike :& getY ptLike

  setPt :: p -> Pt -> p
  setPt ptLike (x :& y) = setX (setY ptLike y) x

instance PtLike (Double, Double) where
  getX (x, _)    = x
  getY (_, y)    = y
  setX (x, y) x' = (x', y)
  setY (x, y) y' = (x, y')

-- instance PtLike (Integer, Integer) where
--   getX (x, _) = fromIntegral x
--   getY (_, y) = fromIntegral y

class Smooth a where
  chaikinStep :: a -> a

  chaikin :: a -> a
  chaikin a = iterate chaikinStep a !! 5

class PtList t where
  (#) :: ([a] -> [a]) -> t a -> t a
  (##) :: ([a] -> b) -> t a -> b
  (%%) :: (t a -> b) -> [a] -> b

----------------- from chaosbox ---------------------
-- | Class of objects that can be queried for points
class Boundary a where
  containsPoint :: a -> Pt -> Bool

class Intersects a b where
  intersectionPoints :: a -> b -> [Pt]
  intersects :: a -> b -> Bool
  intersects a b = null (intersectionPoints a b)

-------------------------------
-- Point

type Pt = Point V2 Double
ptSize = 0.2

infix 5 :&
pattern (:&) x y = P (V2 x y) :: Pt

instance Draw Pt where
  draw (x :& y) = arc x y ptSize 0 (2 * pi)

instance Trail Pt where
  pointsOn p = [p]

instance PtLike Pt where
  getX (x :& _)    = x
  getY (_ :& y)    = y
  setX (x :& y) x' = x' :& y
  setY (x :& y) y' = x  :& y'

point :: Double -> Double -> Pt
point x y = x :& y

-- infix 5 .&
-- (.&) :: Double -> Double -> Pt
-- (.&) = point

randomPt :: (MonadRandom m) => (Double, Double) -> (Double, Double) -> m Pt
randomPt xrange yrange =
  do vec <- randomVec xrange yrange
     return (P vec)

instance Draw [Pt] where
  draw = drawPts
    
drawPts :: [Pt] -> Render ()
drawPts pts = for_ pts $ \pt@(x :& y) -> moveTo (x+ptSize) y *> draw pt *> fill

-------------------------------
-- Vector

type Vec = V2 Double
type VectorField = Pt -> Vec

instance Draw Vec where
  draw v = polyline [0 :& 0, P v]

instance PtLike Vec where
  getX (V2 x _)    = x
  getY (V2 _ y)    = y
  setX (V2 x y) x' = V2 x' y
  setY (V2 x y) y' = V2 x  y'

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

type BoundVec = (Pt, Vec)
(==>) :: Pt -> Pt -> BoundVec
pt1 ==> pt2 = (pt1, pt2 .-. pt1)

arrowAngle = pi/8
arrowRatio = 0.4 :: Double

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

instance Draw BoundVec where
  draw (sx :& sy, v@(V2 dx dy)) = do
    let a = atan2 dx dy - (pi/2)
        -- len = clamp 1 10 (arrowRatio * norm v)
        len = min (arrowRatio * norm v) 10
        fx = sx + dx
        fy = sy + dy
        x1 = fx - len * cos (a - arrowAngle)
        y1 = fy + len * sin (a - arrowAngle)
        x2 = fx - len * cos (a + arrowAngle)
        y2 = fy + len * sin (a + arrowAngle)
    
    -- hsva (norm v * 4) 1 1 1
    moveTo sx sy
    lineTo fx fy
    stroke
    moveTo fx fy
    lineTo x1 y1
    stroke
    moveTo fx fy
    lineTo x2 y2
    stroke

-------------------------------
-- Grid

newtype Grid a = Grid [[(Pt, a)]]
  deriving Show

instance Trail (Grid a) where
  pointsOn (Grid g) = concatMap (map fst) g

-- instance DrawGroup (Grid ()) where
  -- drawFill g = (drawPts . pointsOn) g *> fill
  -- drawFill g = (drawPts . pointsOn) g *> stroke

-- instance DrawGroup t => Grid t where
  -- draw (Grid g) = for_ (concat g) draw

-- instance Draw (Grid Vec) where
--   draw (Grid g) = drawPtVecs (concat g)


drawPtVecs :: [(Pt, Vec)] -> Render ()
drawPtVecs pairs = for_ pairs draw

-- drawVectorField :: Grid Vec -> Render ()
-- drawVectorField (Grid g) = draw

-- drawPtVec :: (Pt, Vec) -> Render ()
-- drawPtVec (x :& y, V2 dx dy)= do
--   moveTo (x - dx/2) (y - dy/2)
--   lineTo (x + dx/2) (y + dy/2)
--   stroke
  
valuesOn :: Grid a -> [a]
valuesOn (Grid g) = concatMap (map snd) g

row :: Int -> Grid a -> [(Pt, a)]
row n (Grid g) = g !! n

column :: Int -> Grid a -> [(Pt, a)]
column n (Grid g) = map (!! n) g

makeGrid :: [Double] -> [Double] -> (Pt -> a) -> Grid a
makeGrid xs ys f = Grid g
  where g = [[(pt, f pt) | x <- xs, let pt = x :& y] | y <- ys]

-------------------------------
-- Polygon

newtype PolygonOf a = PolygonOf [a]
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad)

type Polygon = PolygonOf Pt

pattern Polygon :: [Pt] -> Polygon
pattern Polygon pts = PolygonOf pts
{-# COMPLETE Polygon #-}

instance Draw Polygon where
  draw (Polygon pts) = renderClosedPath pts

instance Trail Polygon where
  pointsOn (Polygon pts) = closed pts

instance Smooth Polygon where
  chaikinStep (Polygon pts) = Polygon (generalChaikinStep $ closed pts)

data Orientation = Colinear | Clockwise | Counterclockwise
orientation :: Pt -> Pt -> Pt -> Orientation
orientation p q r
  | slope (p :~ q) > slope (q :~ r) = Clockwise
  | slope (p :~ q) < slope (q :~ r) = Counterclockwise
  | otherwise                       = Colinear

-- instance Boundary Polygon where
--   containsPoint :: Polygon -> Pt -> Bool
--   containsPoint poly pt
--     | length ## poly < 3 = False
--     | otherwise = loop 0 ((length ## poly) - 1)
--         where loop i 0 = 

-- instance Shape Polygon where
--   randomInside p
--     | length ## p == 3 = randomInsideTriangle p
--     | otherwise = undefined
  
--   boundingBox = (boundingBoxPts ##)
  
boundingBoxPts :: [Pt] -> Polygon
boundingBoxPts ((x1 :& y1) : pts) = boundingBoxPts' x1 x1 y1 y1 pts
  where
    boundingBoxPts' l r t b [] = Polygon [l :& b, l :& t, r :& t, r :& b]
    boundingBoxPts' l r t b ((x :& y) : pts) = boundingBoxPts' (l `min` x) (r `max` x) (t `max` y) (b `min` y) pts

instance PtList PolygonOf where
  f # PolygonOf pts = PolygonOf (f pts)
  f ## PolygonOf pts = f pts
  (%%) = (. PolygonOf)

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

data CircleOf a = CircleOf a Double
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Circle = CircleOf Pt

pattern Circle :: Pt -> Double -> Circle
pattern Circle pt r = CircleOf pt r
{-# COMPLETE Circle #-}

instance Draw Circle where
  draw (Circle (x :& y) r) = arc x y r 0 (2 * pi)

randomInsideCircle (Circle (cx :& cy) radius) = do
  r' <- random
  let r = radius * sqrt r'
  a <- 0 <=> (2 * pi)
  return $ (cx + r * cos a) :& (cy + r * sin a)

-- instance Shape Circle where
  -- Concentrated at center, can also be useful though

  -- randomInside (Circle (cx :& cy) radius) = do
  --   r <- 0 <=> radius
  --   a <- 0 <=> (2 * pi)
  --   return $ (cx + r * cos a) :& (cy + r * sin a)

  -- uniform
  -- randomInside (Circle (cx :& cy) radius) = do
  --   r' <- random
  --   let r = radius * sqrt r'
  --   a <- 0 <=> (2 * pi)
  --   return $ (cx + r * cos a) :& (cy + r * sin a)

  -- boundingBox (Circle center radius) =
  --   square' center radius

-------------------------------
-- Line

data LineOf a = LineOf a a
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Line = LineOf Pt

pattern Line :: Pt -> Pt -> Line
pattern Line pt1 pt2 = LineOf pt1 pt2
{-# COMPLETE Line #-}

pattern (:~) :: Pt -> Pt -> Line
pattern pt1 :~ pt2 = Line pt1 pt2
{-# COMPLETE (:~) #-}

instance Draw Line where
  draw (Line pt1 pt2) = polyline [pt1, pt2] 

instance Trail Line where
  pointsOn (Line pt1 pt2) = [pt1, pt2] 

slope :: Line -> Double
slope ((px :& py) :~ (qx :& qy)) = (qy - py) / (qx - px)

-------------------------------
-- Curve

newtype CurveOf a = CurveOf [a]
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad)

type Curve = CurveOf Pt

pattern Curve :: [Pt] -> Curve
pattern Curve pt = CurveOf pt
{-# COMPLETE Curve #-}

instance Draw Curve where
  draw (Curve pts) = polyline pts

instance Trail Curve where
  pointsOn (Curve pts) = pts

instance Smooth Curve where
  chaikinStep (Curve pts) = Curve (generalChaikinStep pts)

instance PtList CurveOf where
  f # CurveOf pts = CurveOf (f pts)
  f ## CurveOf pts = f pts
  (%%) = (. CurveOf)

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

