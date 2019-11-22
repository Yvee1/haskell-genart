module Genart.Noise where

import Genart.CairoHelpers
import Genart.Shapes
import Numeric.Noise.Perlin

polyAddNoise :: Polygon -> Generate Polygon
polyAddNoise (Polygon pts) = do
  perlinSeed <- fromIntegral <$> asks gcSeed

  let
    perlinOctaves = 5
    perlinScale = 0.1
    perlinPersistance = 0.5
    perlinNoise
      = perlin (round perlinSeed) perlinOctaves perlinScale perlinPersistance
    perlin2d (P (V2 x y))
      = noiseValue perlinNoise (x + perlinSeed, y + perlinSeed, perlinSeed) - 0.5
    addNoise pt = let noise = perlin2d pt in pt .+^ V2 (noise / 5) (noise / 8)

  pure $ Polygon $ map addNoise pts