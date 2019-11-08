{-# LANGUAGE TypeApplications #-}

module Test where

import Genart

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96

darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

renderSketch :: Generate ()
renderSketch = do
  fillScreen eggshell 1
  (w, h) <- getSize @Double

  cairo $ do 
    setLineWidth 0.35
    draw $ pentagon (w/2 .& h/2) 25
    darkGunmetal 1
    fill

test :: IO ()
test = outputSketch (100, 100, 10) renderSketch