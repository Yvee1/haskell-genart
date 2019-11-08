{-# LANGUAGE TypeApplications #-}
module Main where

import Genart

len = 1
ang = 0.1

next :: Int -> Int
next n = if even n then n `div` 2 else ((n*3+1) `div` 2)

collatz :: Int -> [Int]
collatz n = takeWhile (> 1) (iterate next n) ++ [1]

steps :: Int -> Int
steps = (subtract 1) . length . collatz

bg :: Double -> Render ()
-- bg = hsva 355 0.68 0.84
bg = hsva 0 0 0

white :: Double -> Render ()
white = hsva 0 0 100

toCenter :: Generate ()
toCenter = do
  (w, h) <- getSize @Double
  cairo $ translate (w/2) (h)

drawCollatz :: [Int] -> Render ()
drawCollatz ns = do
  matrix <- getMatrix

  for_ ns $ \n -> do
    if (n `mod` 2 == 0)
      then rotate (ang)
      else rotate (-ang) 
    line [V2 0 0, V2 0 (-len)]
    translate 0 (-len)
    white 0.01 *> stroke

  setMatrix matrix

renderSketch :: Generate ()
renderSketch = do
  fillScreen bg 1
  toCenter

  let seqs = map (reverse . collatz) [1..1000]

  for_ seqs $ \seq -> do
    cairo $ do
      setLineWidth 0.1
      drawCollatz seq

main :: IO ()
main = outputSketch (100, 100, 10) renderSketch
