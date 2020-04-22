{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Genart.CairoHelpers
  ( module  Genart.CairoHelpers
  , module  Control.Monad.Reader
  , module  Data.Colour.RGBSpace
  , module  Data.Colour.RGBSpace.HSV
  , module  Data.Foldable            
  , module  GI.Cairo.Render
  , module  Linear.V2
  , module  Linear.Vector
  , module  C
  ) where

import Control.Arrow
import Control.Monad.State
import Control.Monad.Reader
import Data.Random.Source.PureMT
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Foldable            (for_)
import Data.Time.Clock.POSIX
import Data.IORef
import Linear.V2
import Linear.Vector
import Numeric (readHex)
import Data.List.Split (splitOn, chunksOf)
import Text.Printf
import System.Directory (createDirectoryIfMissing)
-- import ChaosBox.AABB         as C
-- import ChaosBox.Affine       as C
import ChaosBox.CLI          as C
-- import ChaosBox.Color        as C
import ChaosBox.Draw         as C
import ChaosBox.Generate     as C
-- import ChaosBox.Geometry     as C
import ChaosBox.Interactive  as C
-- import ChaosBox.Math         as C
-- import ChaosBox.Noise        as C
-- import ChaosBox.Orphanage    as C
-- import ChaosBox.PNG          as C
-- import ChaosBox.Prelude      as C
-- import ChaosBox.Random       as C
import GI.Cairo.Render



fillScreen :: Render a -> Generate ()
fillScreen color = do
  (w, h) <- getSize @Double
  cairo $ do
    rectangle 0 0 w h
    color *> fill

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
 where RGB{..} = hsv h s v

hexa :: String -> Double -> Render ()
hexa s = setSourceRGBA r g b
  where (r, g, b) = hexToRGB s

hexToRGB :: String -> (Double, Double, Double)
hexToRGB s = (r, g, b)
  where [r, g, b] = map ((/255) . fst . head . readHex) $ (chunksOf 2 . last . splitOn "#") s