{-# LANGUAGE TypeApplications #-}

module Test where

import Genart
import Control.Monad
import Data.List (nub)

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96

darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

vividTangerine :: Double -> Render ()
vividTangerine = hsva 11 0.40 0.92

englishVermillion :: Double -> Render ()
englishVermillion = hsva 355 0.68 0.84

grid :: IO ()
grid = outputSketch (100, 100, 10, False) $ do
  fillScreen $ darkGunmetal 1
  (w, h) <- getSize @Double
  let c = w/2 :& h/2

  cairo $ do 
    setLineWidth 0.35
    
    let lgap = 1


    let path1 = (-30) :& (-30) ~~ 15 :& 20 ~~ 40 :& 25 ~~ 70 :& 15 ~~ 90 :& 30 ~~ 200 :& 160
    let newPath1 = iterate chaikinStep path1 !! 8

    pushGroup
    rectangle 0 0 w h
    darkGunmetal 1 *> fill
    translate 0 10
    eggshell 1
    sequence_ [draw newPath1 *> translate 0 lgap *> stroke| _ <- [0..20]]
    popGroupToSource

    sequence_ [draw (square (x :& y) 3) *> fill | x <- [5, 10..95], y <- [5, 10..95]]
  
perron :: IO ()
perron = outputSketch (100, 100, 10, False) $ do
    (w, h) <- getSize @Double
    let bg = hsva 0 0 0.12 1
    let c = w/2 :& h/2
    fillScreen bg

    pts <- replicateM 300000 $ randomPt (0, 100) (0, 100)

    cairo $ do
      let segs = segments $
                ((18 :& 35) ~~ (21 :& 31)   ~~ (23 :& 29) ~~
                 (25 :& 28) ~~ (27 :& 27.5) ~~ (29 :& 28) ~~
                 (33 :& 30) ~~ (35 :& 30))  ~~ (85 :& 30)
      let lines = segs ++ [Line (15 :& y) (85 :& y) | y <- [10, 20..90], y /= 30]
      -- crappy <- mapM deteriorate lines

      white 0.03
      mapM_ (\pt -> draw (circle pt 0.1) *> fill) pts
      setLineWidth 0.8
      setLineJoin LineJoinBevel
      hsva 0 0 0.8 1
      mapM_ (\l -> draw l *> stroke) lines

field :: VectorField
-- field v@(x' :& y') = sin x ^& sin (min x (exp x) - min (sin (norm v)) (max x y))
field (x' :& y') = sin x ^& sin (min x (exp x))
  where x = x' / 10
        y = y' / 10

test :: IO ()
test = outputSketch (100, 100, 15, False) $ do
  (w, h) <- getSize @Double
  let bg = hsva 0 0 0.9

  let c = w/2 :& h/2
  fillScreen (bg 1)

  cairo $ do
    translate (getX c) (getY c)
    setLineWidth 0.2
    black 1

    let a = makeGrid [-40..40] [-40..40] field
    draw a

randomWalk :: Circle -> Generate ()
randomWalk s = do
  pts <- replicateM 10 $ randomInside s
  cairo $ mapM_ (\pt -> draw (circle pt 0)) pts