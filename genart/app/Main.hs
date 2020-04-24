module Main where

import Sketches
import GI.Cairo.Render

sketch = svgTest

main :: IO ()
main = Main.test

mySketch :: Render ()
mySketch = do
  setSourceRGBA 1 1 0 1
  rectangle 10 10 100 100
  fill

test :: IO ()
test = 
  withSVGSurface "out.svg" 500 500 $ \surface -> do
    renderWith surface mySketch
    surfaceFinish surface