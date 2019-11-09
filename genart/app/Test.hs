{-# LANGUAGE TypeApplications #-}

module Test where

import Genart
import Control.Monad

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96

darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

renderSketch :: Generate ()
renderSketch = do
  fillScreen eggshell 1
  (w, h) <- getSize @Double
  let c = w/2 .& h/2

  cairo $ do 
    setLineWidth 0.35
    let t = triangle c 40
    draw $ iterate chaikinPolygonStep t !! 5
    darkGunmetal 1 *> fill

test :: IO ()
test = outputSketch (100, 100, 10) renderSketch