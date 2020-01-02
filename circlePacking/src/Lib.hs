{-# LANGUAGE TypeApplications #-}
module Lib where

import Genart

type GrowingCircle = (Circle, Bool)

growList :: [Circle] -> Generate [GrowingCircle]
growList (c : cs) = do
  let newC = increaseSize c 0.1
  inside <- insideScreen newC
  allowed <- if inside then c `checkOverlapping` cs else False
  

grow :: Circle -> Generate GrowingCircle
grow c = do
  let newC = increaseSize c 0.1
  inside <- insideScreen newC
  if inside then return (newC, True) else return (c, False)

increaseSize :: Circle -> Double -> Circle
increaseSize (Circle center radius) dr = Circle center (radius + dr)

insideScreen :: Circle -> Generate Bool
insideScreen (Circle (x :& y) r) = do
  (w, h) <- getSize @Double
  return (x + r <= w && x - r >= 0 && y + r <= h && y - r >= 0)

takeWhileM :: (Monad m) => (a -> Bool) -> [m a] -> m [a]
takeWhileM _ [] = return []
takeWhileM p (a:as) =
   do v <- a
      if p v
         then do vs <- takeWhileM p as
                 return (v:vs)
         else return []

drawPointsGrid :: Double -> Double -> Double -> Render ()
drawPointsGrid w h s = do
  let g1 = makeGrid [0..w] [0, 2..h] (const ())
  let g2 = makeGrid [0.5, 1.5..w] [1, 3..h] (const ())
  mapM_ (\pt -> draw (circle pt s) *> fill) (pointsOn g1 ++ pointsOn g2)

drawWithWhiteDots :: Generate ()
drawWithWhiteDots = do
  (w, h) <- getSize @Double
  cairo pushGroup
  fillScreen (black 1)
  s <- 0.2 <=> 0.4

  cairo $ do
    white 1
    drawPointsGrid w h s
    popGroupToSource
  
drawWithBlackDots :: Generate ()
drawWithBlackDots = do
  (w, h) <- getSize @Double
  cairo pushGroup
  fillScreen (white 1)
  s <- 0.1 <=> 0.5

  cairo $ do
    black 1
    drawPointsGrid w h s
    popGroupToSource

green :: Render ()
green = hsva 90 0.65 0.6 1
  
drawWithGreenDots :: Generate ()
drawWithGreenDots = do
  (w, h) <- getSize @Double
  cairo pushGroup
  fillScreen green
  cairo $ do
    black 1
    drawPointsGrid w h 0.25
    popGroupToSource

makeCircle :: Generate Circle
makeCircle = do
  (w, h) <- getSize @Double
  loc <- randomPt (1, w-1) (1, h-1)
  let c = circle loc 0
  futureCs <- takeWhileM snd $ iterate (>>= (\(newC, growing) -> grow newC)) (grow c)
  return . fst . last $ futureCs

-- makeCircle :: Generate Circle
-- makeCircle = do
--   c <- tryCircle
--   inside <- insideScreen c
--   if inside then return c else makeCircle

-- tryCircle :: Generate Circle
-- tryCircle = do
--   (w, h) <- getSize @Double
--   loc <- randomPt (0, w) (0, h)
--   r <- 2 <=> 10
--   return $ circle loc r

drawCircle :: Circle -> Generate ()
drawCircle c = do
  r <- 0 <=> 1
  if (r :: Int) == 0 then drawWithBlackDots else drawWithWhiteDots
  cairo $ do
    draw c
    fill
