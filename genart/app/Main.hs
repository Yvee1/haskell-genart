{-# LANGUAGE TypeApplications #-}

module Main where

import Genart
import Data.List (nub)
import Data.Random (uniform, randomElement)
import Data.RVar

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96

fromIntegralVector :: V2 Int -> V2 Double
fromIntegralVector (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

genQuadGrid :: Generate [Polygon]
genQuadGrid = do
  (w, h) <- getSize @Int
  points <- replicateM 800 $ do
    v <- V2 <$> sampleRVar (uniform 3 (w `div` 2 - 3)) <*> sampleRVar (uniform 3 (h `div` 2 - 3))
    let pt = P $ fromIntegralVector v
    pure $ pt * 2
  pure . nub . flip map points $ \pt ->
    Polygon [pt, pt .+^ V2 0 1.5, pt .+^ V2 1.5 1.5, pt .+^ V2 1.5 0]
    
darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

teaGreen :: Double -> Render ()
teaGreen = hsva 81 0.25 0.94

vividTangerine :: Double -> Render ()
vividTangerine = hsva 11 0.40 0.92

englishVermillion :: Double -> Render ()
englishVermillion = hsva 355 0.68 0.84

renderSketch :: Generate ()
renderSketch = do
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

main :: IO ()
main = outputSketch (60, 60, 20, False) renderSketch
