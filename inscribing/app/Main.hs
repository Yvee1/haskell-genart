{-# LANGUAGE TypeApplications #-}

module Main where

import Genart
import Control.Monad

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96

darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

once :: Pt -> Double -> Int -> Render Double
once c r n =
  do
    let p = ngon n c r
    draw p 
    eggshell 1
    fill

    draw $ incircle p 
    darkGunmetal 1
    fill

    return $ inradius p

once' c r n = 
  do
    let a = circle c r
    draw a
    eggshell 1
    fill

    let p = ngon n c r
    draw p
    darkGunmetal 1
    fill
    
    return $ inradius p

renderSketch :: Generate ()
renderSketch = do
  fillScreen darkGunmetal 1
  (w, h) <- getSize @Double
  let c = w/2 .& h/2

  cairo $ do 
    setLineWidth 0.35
    foldM_ (once c) 40 [40, 39..3]
    -- foldM_ (once' c) 40 [3..50]

main :: IO ()
main = outputSketch (100, 100, 10) renderSketch