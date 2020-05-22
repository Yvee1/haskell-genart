module Lib where

import Genart

drawFaceAt :: Pt -> Double -> Generate ()
drawFaceAt c r = do
  cairo $ do
    black 1
    -- draw c
    -- fill
    -- draw $ circle c r
    -- stroke
    -- setSourceRGB 1 0 0

  let dx = r/4
      dy = -r/5
      eye1 = c + (-dx :& dy)
      eye2 = c + (dx  :& dy)

      mouth = c + (0 :& r/2)

  drawEyeAt eye1 (r/2)
  drawEyeAt eye2 (r/2)

  drawNoseAt c (r/8)

  drawMouthAt mouth (r/2)

drawEyeAt :: Pt -> Double -> Generate ()
drawEyeAt pt h = cairo $ do
  draw $ pt :~ (pt - (0 :& h))
  stroke

drawNoseAt :: Pt -> Double -> Generate ()
drawNoseAt c s = cairo $ do
  draw . chaikin . fmap (+ (0:& -s/2)) $ (c + (s :& -s)) ~~ (c - (s :& -s)) ~~ (c + (2*s :& 0))
  stroke

drawMouthAt :: Pt -> Double -> Generate ()
drawMouthAt (x :& y) r = cairo $ do
  arc x (y-r*0.8) r 0.4 (pi-0.4)
  stroke