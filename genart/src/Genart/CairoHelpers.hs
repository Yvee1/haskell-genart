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

import Control.Arrow
import Control.Monad.State
import Control.Monad.Reader
import Data.Random.Source.PureMT
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Foldable            (for_)
import Data.Time.Clock.POSIX
import Data.IORef
import Graphics.Rendering.Cairo
import Linear.V2
import Linear.Vector
import Numeric (readHex)
import Data.List.Split (splitOn, chunksOf)
import Text.Printf
import System.Directory (createDirectoryIfMissing)

data GenerateContext = GenerateContext
  { gcWidth          :: Int
  , gcHeight         :: Int
  , gcSeed           :: Int
  , gcScale          :: Double
  -- , gcName           :: String
  , gcRenderProgress :: Bool
  , gcProgress       :: IORef Int
  -- , gcBeforeSaveHook
  }

type Generate a = StateT PureMT (ReaderT GenerateContext Render) a

-- | Lift a Cairo action into a Generate action
cairo :: Render a -> Generate a
cairo = lift . lift

getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (gcWidth &&& gcHeight)
  pure (fromIntegral w, fromIntegral h)

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

outputSketch :: (Int, Int, Double, Bool) -> Generate () -> IO ()
outputSketch (w, h, s, renderAnimate) sketch = do
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
  counter <- newIORef 0
  let ctx = GenerateContext width height (fromIntegral seed) scaleAmount renderAnimate counter

  void
    (runGenerate surface ctx rng sketch)
    -- . renderWith surface
    -- . flip runReaderT world
    -- . flip runStateT rng
    -- $ do
    --   cairo $ scale scaleAmount scaleAmount
    --   sketch

  putStrLn "Generating art..."

  liftIO $ createDirectoryIfMissing True "images"

  surfaceWriteToPNG surface
    $ "images/"
    ++ show seed ++ "-" ++ show (round scaleAmount :: Int) ++ ".png"
  surfaceWriteToPNG surface "images/latest.png"

renderProgress :: Generate ()
renderProgress = do
  doRender <- asks gcRenderProgress
  when doRender $ do
    let padInt :: Int -> String
        padInt = printf "%.8v"

    progressRef <- asks gcProgress
    seed <- asks gcSeed
    scaleAmount <- asks gcScale
    progress    <- liftIO $ readIORef progressRef

    cairo . withTargetSurface $ \surface -> do
      liftIO . putStrLn $ "Rendering progress surface #" <> show progress
      let folder = "images/" <> "progress/" <> (show seed <> "-" <> show (round scaleAmount :: Int))


      liftIO $ createDirectoryIfMissing True folder
      liftIO
        $  surfaceWriteToPNG surface
        $  folder <> "/" <> padInt progress
        <> ".png"

    liftIO $ modifyIORef progressRef (+ 1)

runGenerate :: Surface -> GenerateContext -> PureMT -> Generate a -> IO (a, PureMT)
runGenerate surface ctx@GenerateContext {..} rng doRender =
  renderWith surface
    . flip runReaderT ctx
    . flip runStateT  rng

    $ do
        cairo $ scale gcScale gcScale
        doRender