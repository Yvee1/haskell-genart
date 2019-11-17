{-# LANGUAGE TypeApplications #-}

module Test where

import Genart
import Control.Monad
import Data.List (nub)

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96

darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

vividTangerine :: Double -> Render ()
vividTangerine = hsva 11 0.40 0.92

englishVermillion :: Double -> Render ()
englishVermillion = hsva 355 0.68 0.84

grid :: IO ()
grid = outputSketch (100, 100, 10) $ do
  fillScreen darkGunmetal 1
  (w, h) <- getSize @Double
  let c = w/2 .& h/2

  cairo $ do 
    setLineWidth 0.35
    
    let lgap = 1


    let path1 = (-30) .& (-30) ~~ 15 .& 20 ~~ 40 .& 25 ~~ 70 .& 15 ~~ 90 .& 30 ~~ 200 .& 160
    let newPath1 = iterate chaikinStep path1 !! 8

    pushGroup
    rectangle 0 0 w h
    darkGunmetal 1 *> fill
    translate 0 10
    eggshell 1
    sequence_ [draw newPath1 *> translate 0 lgap *> stroke| _ <- [0..20]]
    popGroupToSource

    sequence_ [draw (square (x .& y) 3) *> fill | x <- [5, 10..95], y <- [5, 10..95]]
  

color r
  | r < 0.5 = hexa "4dd599" 1 
  | r < 1 = hexa "FFDC30" 1
  | otherwise = hexa "00918e" 1
-- color r
--   | r < 0.5 = hexa "fff3fb" 1 
--   | r < 1 = hexa "a5638f" 1
--   | otherwise = hexa "ffd2ea" 1


test :: IO ()
test = do 
  let s = ngon 3 (50 .& 58) 40
  let initial = triangulateConvexPolygonFromCenter s
  let triangles = iterate (concatMap triangulateConvexPolygonFromCenter) initial !! 7
  let circles = map incircle triangles
  let radii = nub $ map (\(Circle _ r) -> r) circles
  print $ minimum radii

  outputSketch (100, 100, 10) $ do
    -- fillScreen (hexa "110133") 1
    fillScreen (hexa "002b70") 1
    (w, h) <- getSize @Double
    let c = w/2 .& h/2

    cairo $ do
      setLineWidth 0.1

      -- mapM_ (\t -> draw t *> hsva 100 0.5 0.6 1 *> strokePreserve *> darkGunmetal 1 *> fill) triangles
      mapM_ (\t -> draw t *> white 0.05 *> stroke) triangles
      
      -- mapM_ (\c@(Circle _ r) -> draw c *> color r *> fill) circles
      -- mapM_ (\c@(Circle _ r) -> draw c *> if r < 1 then pure () else white 1 *> fill) circles

        
      

  putStrLn "done"