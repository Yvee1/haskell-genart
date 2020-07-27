module Main where

import Lib
import Genart

main :: IO ()
main = runChaosBoxWith (\o -> o { optWidth = 60, optHeight = 60, optScale = 20 }) $ do
  c <- getCenter
  let bg = white 1

  cairo $ setLineWidth 0.01

  -- eventLoop $ do
  fillScreen bg

  -- drawFaceAt c 30
  forM_ (pointsOn (makeGrid [5, 15..60] [5, 15..60])) (`drawFaceAt` 0.5)