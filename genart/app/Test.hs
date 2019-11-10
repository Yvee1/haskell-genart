{-# LANGUAGE TypeApplications #-}

module Test where

import Genart
import Control.Monad

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96

darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

vividTangerine :: Double -> Render ()
vividTangerine = hsva 11 0.40 0.92

englishVermillion :: Double -> Render ()
englishVermillion = hsva 355 0.68 0.84

grid :: IO ()
grid = outputSketch (100, 100, 10) $ do
  fillScreen darkGunmetal 1
  (w, h) <- getSize @Double
  let c = w/2 .& h/2

  cairo $ do 
    setLineWidth 0.35
    
    let lgap = 1


    let path1 = (-30) .& (-30) ~~ 15 .& 20 ~~ 40 .& 25 ~~ 70 .& 15 ~~ 90 .& 30 ~~ 200 .& 160
    let newPath1 = iterate chaikinStep path1 !! 8

    pushGroup
    rectangle 0 0 w h
    darkGunmetal 1 *> fill
    translate 0 10
    eggshell 1
    sequence_ [draw newPath1 *> translate 0 lgap *> stroke| _ <- [0..20]]
    popGroupToSource

    sequence_ [draw (square (x .& y) 3) *> fill | x <- [5, 10..95], y <- [5, 10..95]]
    
test :: IO ()
test = do 
  
  outputSketch (100, 100, 10) $ do
    fillScreen white 1
    (w, h) <- getSize @Double
    let c = w/2 .& h/2

    cairo $ do
      setLineWidth 0.35

      -- let path2 = (-30) .& 130 ~~ 10 .& 90 ~~ 30 .& 65 ~~ 60 .& 15 ~~ 90 .& 5 ~~ 120 .& (-20)
      -- draw path2
      -- black 1
      -- stroke
      let s = square c 50
      draw $ chaikin s
      black 1
      stroke

  putStrLn "Done"