{-# LANGUAGE TypeApplications #-}

module Sketches where

import Genart
import Control.Monad
import Data.List (nub)
import Data.Random (uniform, randomElement)
import Data.RVar
import Control.Monad.IO.Class (liftIO)

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96

darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

vividTangerine :: Double -> Render ()
vividTangerine = hsva 11 0.40 0.92

englishVermillion :: Double -> Render ()
englishVermillion = hsva 355 0.68 0.84

grid :: IO ()
grid = runChaosBoxWith (\opt -> opt {optWidth = 100, optHeight = 100, optScale=10}) $ eventLoop $ do
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
perron = runChaosBoxWith (\opt -> opt {optWidth = 100, optHeight = 100, optScale=10}) $ eventLoop $ do
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

fabric :: VectorField
-- field v@(x' :& y') = sin x ^& sin (min x (exp x) - min (sin (norm v)) (max x y))
fabric (x' :& y') = sin x ^& sin (min x (exp x))
  where x = x' / 10
        y = y' / 10

electric :: Pt -> Double -> VectorField
electric pos charge pt = if d /= 0 then charge *^ v ^/ (d ** 3) else 0 ^& 0
  where v = pt .-. pos
        d = norm v


drawPointsGrid :: Double -> Double -> Double -> Render ()
drawPointsGrid w h s = do
  let g1 = makeGrid [0..w] [0, 2..h] (const ())
  let g2 = makeGrid [0.5, 1.5..w] [1, 3..h] (const ())
  mapM_ (\pt -> draw (circle pt s) *> fill) (pointsOn g1 ++ pointsOn g2)

test :: IO ()
test = runChaosBoxWith (\opt -> opt {optWidth = 100, optHeight = 100, optScale=10}) $ eventLoop $ do
  (w, h) <- getSize @Double
  let beige = hsva 0 0 0.9
  let bg = beige
  -- let bg = black

  let c = w/2 :& h/2
  fillScreen (bg 1)
  let p = square' c 40
  let ell = circle 0 20

  cairo $ do
    -- translate (getX c) (getY c)
    setLineWidth 0.2
    black 1

    pushGroup
  -- fillScreen (white 1)
  cairo $ do
    drawPointsGrid w h 0.35
    popGroupToSource

    save
    translate (getX c) (getY c)
    -- rotate (pi/2)
    scale 1.3 1.8
    draw ell
    restore
    fill
    -- save
    -- translate (getX c) (getY c)
    -- rotate (pi/2)
    -- scale 1.3 1.8
    -- draw ell
    -- restore
    -- fill
    pushGroup
  fillScreen (black 1)
  cairo $ do
    -- beige 1
    drawPointsGrid w h 0.25
    popGroupToSource
    mapM_ (\pt -> draw (circle pt 6) *> fill) (pointsOn p)

    -- draw $ (0 :& 0) ==> (4 :& 5)
    -- liftIO $ print a
    -- draw a
    -- stroke

randomWalk :: Circle -> Generate ()
randomWalk s = do
  pts <- replicateM 10 $ randomInsideCircle s
  cairo $ mapM_ (\pt -> draw (circle pt 0)) pts

fromIntegralVector :: V2 Int -> V2 Double
fromIntegralVector (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

genQuadGrid :: Generate [Polygon]
genQuadGrid = do
  (w, h) <- getSize @Int
  points <- replicateM 800 $ do
    v <- V2 <$> (3 <=> (w `div` 2 - 3)) <*> (3 <=> (h `div` 2 - 3))
    let pt = P $ fromIntegralVector v
    pure $ pt * 2
  pure . nub . flip map points $ \pt ->
    Polygon [pt, pt .+^ V2 0 1.5, pt .+^ V2 1.5 1.5, pt .+^ V2 1.5 0]
    
teaGreen :: Double -> Render ()
teaGreen = hsva 81 0.25 0.94

kovach :: IO ()
kovach = runChaosBoxWith (\o -> o { optWidth = 60, optHeight = 60, optScale = 20 }) $ eventLoop $ do
  fillScreen $ eggshell 1

  cairo $ setLineWidth 0.15

  quads <- genQuadGrid
  noisyQuads <- traverse polyAddNoise quads

  for_ noisyQuads $ \quad -> do
    -- strokeOrFill <- sampleRVar $ weightedElement [(0.4, fill), (0.6, stroke)]
    strokeOrFill <- sampleRVar $ randomElement [fill, stroke]
    color <- sampleRVar $ randomElement
       [ teaGreen
       , vividTangerine
       , englishVermillion
       , darkGunmetal
       ]
    cairo $ do
      draw quad
      color 1 *> strokeOrFill


svgTest = runChaosBoxWith (\o -> o { optWidth = 60, optHeight = 60, optScale = 20 }) $ do
  (w, h) <- getSize @Double
  let c = w/2 :& h/2

  flame <- cairo $ svg "flame.svg" 10
  let (flameW, flameH) = dimensionsOfSvg flame
  let s = rect (c + (flameW/2 :& flameH/2)) flameW flameH
  
  cairo $ setLineWidth 0.15

  eventLoop $ do
    fillScreen $ black 1
    cairo $ do

      -- m <- getMatrix

      draw s
      white 1
      stroke

      drawSvg flame c
      -- translate (w/2) (h/2)
      -- scale 0.5 0.5


    -- setMatrix m