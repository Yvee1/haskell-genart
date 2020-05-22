module Main where

import Lib
import Genart

main :: IO ()
main = runChaosBoxWith (\o -> o { optWidth = 60, optHeight = 60, optScale = 20 }) $ do
  c <- getCenter
  let bg = white 1

  cairo $ setLineWidth 0.1

  -- eventLoop $ do
  fillScreen bg

  drawFaceAt c 30
