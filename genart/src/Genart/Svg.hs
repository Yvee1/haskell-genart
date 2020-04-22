module Genart.Svg where

import Genart.CairoHelpers
import GI.Cairo.Render.LibRSvg
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (liftIO)

loadSvg :: FilePath -> IO Svg
loadSvg file = do
  svgOrError <- fromBuffer =<< BS.readFile file
  case svgOrError of
    Left err -> error $ show err
    Right svg -> return svg

instance Draw Svg where
  draw svg = do
    succeeded <- render svg
    if succeeded then return () else error "Drawing of SVG failed"

dimensionsOfSvg :: Svg -> IO (Int, Int)
dimensionsOfSvg = dimensions

setDpiOfSvg :: Svg -> Double -> IO ()
setDpiOfSvg = setDpi

setDpiXYOfSvg :: Svg -> Double -> Double -> IO ()
setDpiXYOfSvg = setDpiXY