{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Genart.CairoHelpers
  ( module  Genart.CairoHelpers
  , module  Control.Monad.Reader
  , module  Data.Colour.RGBSpace
  , module  Data.Colour.RGBSpace.HSV
  , module  Data.Foldable            
  , module  Graphics.Rendering.Cairo
  , module  Linear.V2
  , module  Linear.Vector
  ) where

import           Control.Arrow
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.Random.Source.PureMT
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSV
import           Data.Foldable            (for_)
import           Data.Time.Clock.POSIX
import           Graphics.Rendering.Cairo
import           Linear.V2
import           Linear.Vector

data World = World
  { worldWidth  :: Int
  , worldHeight :: Int
  , worldSeed   :: Int
  , worldScale  :: Double
  }

type Generate a = StateT PureMT (ReaderT World Render) a

-- | Lift a Cairo action into a Generate action
cairo :: Render a -> Generate a
cairo = lift . lift

getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (worldWidth &&& worldHeight)
  pure (fromIntegral w, fromIntegral h)

fillScreen :: (Double -> Render a) -> Double -> Generate ()
fillScreen color opacity = do
  (w, h) <- getSize @Double
  cairo $ do
    rectangle 0 0 w h
    color opacity *> fill

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
 where RGB{..} = hsv h s v

outputSketch :: (Int, Int, Double) -> Generate () -> IO ()
outputSketch (w, h, s) sketch = do
  seed <- round . (*1000) <$> getPOSIXTime
  let
    rng = pureMT seed
    width = w
    height = h
    scaleAmount = s

    scaledWidth = round $ fromIntegral width * scaleAmount
    scaledHeight = round $ fromIntegral height * scaleAmount

  surface <- createImageSurface FormatARGB32 scaledWidth scaledHeight
  -- The "world" thinks the width and height are the initial values, not scaled.
  let world = World width height (fromIntegral seed) scaleAmount

  void
    . renderWith surface
    . flip runReaderT world
    . flip runStateT rng
    $ do
      cairo $ scale scaleAmount scaleAmount
      sketch

  putStrLn "Generating art..."
  surfaceWriteToPNG surface
    $ "images/"
    ++ show seed ++ "-" ++ show (round scaleAmount :: Int) ++ ".png"
  surfaceWriteToPNG surface "images/latest.png"
