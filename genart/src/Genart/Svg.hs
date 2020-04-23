{-# LANGUAGE TypeApplications #-}
module Genart.Svg where

import Genart.CairoHelpers
import Genart.Shapes.Types (PtLike, getX, getY)
import GI.Cairo.Render.LibRSvg
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (liftIO)

data SVG = SVG Svg Double Double

svg :: FilePath -> Double -> Render SVG
svg file maxSize = do
  svg <- loadSvg' file
  (w, h) <- dimensionsOfSvg' svg
  let s = maxSize / max w h
  return $ SVG svg (s * w) (s * h)

loadSvg :: FilePath -> Render SVG
loadSvg file = do
  svg <- loadSvg' file
  (w, h) <- dimensionsOfSvg' svg
  return $ SVG svg w h

loadSvg' :: FilePath -> Render Svg
loadSvg' file = liftIO $ do
  svgOrError <- fromBuffer =<< BS.readFile file
  case svgOrError of
    Left err -> error $ show err
    Right svg -> return svg

drawSvg :: PtLike p => SVG -> p -> Render ()
drawSvg (SVG svg w' h') center = do
  (w, h) <- dimensionsOfSvg' svg
  m <- getMatrix
  -- translate (-w / 2) (-h / 2)
  translate (getX center) (getY center)
  scale (w' / w) (h' / h)
  -- liftIO . print $ (w, h)
  -- liftIO . print $ maxSize / max w h
  draw svg
  setMatrix m

instance Draw Svg where
  draw svg = do
    succeeded <- render svg
    if succeeded then return () else error "Drawing of SVG failed"

dimensionsOfSvg :: SVG -> (Double, Double)
dimensionsOfSvg (SVG _ w h) = (w, h)

dimensionsOfSvg' :: Num a => Svg -> Render (a, a)
dimensionsOfSvg' svg = liftIO $ do
  (w, h) <- liftIO $ dimensions svg
  return (fromIntegral w, fromIntegral h)

setDpiOfSvg :: Svg -> Double -> IO ()
setDpiOfSvg = setDpi

setDpiXYOfSvg :: Svg -> Double -> Double -> IO ()
setDpiXYOfSvg = setDpiXY