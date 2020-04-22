{-# LANGUAGE TypeApplications #-}

module Main where

import Lib
import Genart

main :: IO ()
main = outputSketch (100, 100, 10, False) $ do
  (w, h) <- getSize @Double
  let center = w/2 :& h/2
  let bg = white 1
  fillScreen bg 

  liftIO $ print "HI0"

  -- drawWithBlackDots
  cairo $ do
    white 1
    draw $ Polygon [0 :& 0, w :& 0, w :& h]
    fill

  -- drawWithWhiteDots
  cairo $ do
    black 1
    draw $ Polygon [0 :& 0, w :& h, 0 :& h]
    fill

  liftIO $ print "HI1"
  circles <- sequence [makeCircle | _ <- [1..40]]
  liftIO $ print "HI2"
  mapM_ drawCircle circles
