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

  mouth <- tweak 0.2 $ c + (0 :& r/2)

  drawEyeAt eye1 (r/2)
  drawEyeAt eye2 (r/2)

  drawNoseAt c (r/8)

  typeOfMouth <- unsafeSample [drawUnhappyMouthAt, drawHappyMouthAt, drawNeutralMouthAt]
  typeOfMouth mouth (r/2)
  -- drawNeutralMouthAt mouth (r/2)

drawEyeAt :: Pt -> Double -> Generate ()
drawEyeAt pt h = do
  let straight = pt :~ (pt - (0 :& h))
  wobblyLine <- wobbleLine 0.8 =<< slant 1 straight
  cairo $ black 0.1
  drawWithTexture 20 4 0.15 wobblyLine 
  -- cairo $ do
  --   black 1
  --   draw wobblyLine
  --   stroke

drawNoseAt :: Pt -> Double -> Generate ()
drawNoseAt c s = do
  let straightCurve = fmap (+ (0:& -s/2)) $ (c + (s :& -s)) ~~ (c - (s :& -s)) ~~ (c + (2*s :& 0))
  wobblyCurve <- wobbleCurve 0.8 (chaikinStep straightCurve)
  cairo $ black 0.1
  drawWithTexture 20 2 0.15 wobblyCurve
  -- cairo $ do
  --   black 1
  --   draw wobblyCurve 
  --   stroke

drawHappyMouthAt :: Pt -> Double -> Generate ()
drawHappyMouthAt (x :& y) r = do
  r1 <- normal 0 0.1
  r2 <- normal 0 0.1
  let straightCurve = arcCurve 25 (x :& (y-r*0.8)) r (0.4 + r1) (pi-0.4 + r2)
  wobblyCurve <- wobbleCurve 0.1 straightCurve
  cairo $ black 0.1
  drawWithTexture 20 2 0.15 wobblyCurve

drawUnhappyMouthAt :: Pt -> Double -> Generate ()
drawUnhappyMouthAt (x :& y) r = do
  r1 <- normal 0 0.1
  r2 <- normal 0 0.1
  let straightCurve = arcCurve 25 (x :& (y+r*0.5)) r (pi+0.4+r2) (2*pi-0.4+r1) 
  wobblyCurve <- wobbleCurve 0.2 straightCurve
  cairo $ black 0.1
  drawWithTexture 20 2 0.15 wobblyCurve

drawNeutralMouthAt :: Pt -> Double -> Generate ()
drawNeutralMouthAt (x :& y) r = do
  let straight = (x - r :& y - 0.2*r) :~ (x + r :& y-0.2*r)
  wobbliness <- normal 1.2 1
  wobbly <- wobbleLine wobbliness =<< slant 2 straight
  cairo $ black 0.1
  drawWithTexture 20 4 0.15 wobbly

slant :: Double -> Line -> Generate Line
slant sigma (pt1 :~ pt2) = do
  pt1' <- tweak sigma pt1
  pt2' <- tweak sigma pt2
  return (pt1' :~ pt2')

tweak :: Double -> Pt -> Generate Pt
tweak sigma (x :& y) = do
  x' <- normal x sigma
  y' <- normal y sigma
  return (x' :& y')

wobbleLine :: Double -> Line -> Generate Curve
wobbleLine sigma l@(pt1 :~ pt2) = do
  let pts = subpoints 5 l
  pts' <- wobblePts sigma pts
  return (pt1 ~~ Curve pts' ~~ pt2)

wobbleCurve :: Double -> Curve -> Generate Curve
wobbleCurve sigma (Curve pts) = Curve <$> wobblePts sigma pts

wobblePts :: Double -> [Pt] -> Generate [Pt]
wobblePts sigma pts = smoothen <$> mapM (tweak sigma) pts

smoothen :: [Pt] -> [Pt]
smoothen pts = map ((/2) . uncurry (+)) (zip pts (tail pts))

drawWithTexture :: Int -> Int -> Double -> Curve -> Generate ()
drawWithTexture steps res sigma curve = 
  forM_ [0..steps] $ const $ drawWithTextureStep res sigma curve

drawWithTextureStep res sigma (Curve pts) = do
  pts' <- chaikin <$> mapM (tweak sigma) pts
  let pts'' = concatMap (subpoints res . uncurry Line) (zip pts' (tail pts'))
  cairo $ mapM ((fill <*) . draw . flip circle 0.05) pts''
  return ()