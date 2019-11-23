{-# LANGUAGE TypeApplications #-}

module Main where

import Lib
import Genart
import Control.Monad

drawSquare :: Double -> Double -> Generate (Render ())
drawSquare x y = do
  (w, h) <- getSize @Double
  return $ do
    setSourceRGBA 1 1 1 1
    rectangle x y 2 2
    fill

sketch :: Generate ()
sketch = do
  startState <- start 800 50 50 0 (-1)
  let trunk = reach startState
  (branches, sources) <- foldr (<=<) return (replicate 50 step) trunk

  let lines = map branchToLine branches

  fillScreen black 1
  cairo $ do
    setLineWidth 0.15
    setLineCap LineCapRound

    -- setSourceRGBA 1 0 0 1
    -- mapM_ (\s -> draw s *> fill) (snd startState)

    setSourceRGBA 0 1 0 1
    mapM_ (\s -> draw s *> fill) sources

    white 1

  mapM_ (\l -> cairo (draw l) *> cairo stroke *> renderProgress) lines

main :: IO ()
main = outputSketch (100, 100, 10, False) sketch 