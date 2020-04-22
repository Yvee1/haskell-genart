{-# LANGUAGE TypeApplications #-}

module Main where

import Lib
import Genart
import Control.Monad

sketch :: Generate ()
sketch = do
  startState <- start 500 50 50 0 (-1)
  let trunk = reach startState
  (branches, sources) <- foldr (<=<) return (replicate 300 step) trunk

  let lines = map branchToLine branches

  fillScreen $ black 1
  cairo $ do
    setLineWidth 0.15
    setLineCap LineCapRound

    -- setSourceRGBA 1 0 0 1
    -- mapM_ (\s -> draw s *> fill) (snd startState)

    setSourceRGBA 0 1 0 1
    mapM_ (\s -> draw s *> fill) sources

    white 1

  mapM_ (\l -> cairo (draw l) *> cairo stroke) lines

main :: IO ()
main = runChaosBoxWith (\o -> o {optWidth = 100, optHeight = 100, optScale=10}) (eventLoop sketch)