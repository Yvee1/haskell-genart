{-# LANGUAGE FlexibleContexts  #-}
module Genart.Random where

import Genart.CairoHelpers
import Data.Random
import Data.RVar

-- random :: Distribution Uniform a => a -> a -> Generate a
-- random a b = sampleRVar $ uniform a b

random :: (MonadRandom m) => m Double
random = sampleRVar $ uniform 0 1

-- randoms :: (MonadRandom m) => m [Double]
-- randoms = repeat random

(<=>) :: (MonadRandom m, Distribution Uniform a) => a -> a -> m a
a <=> b = sampleRVar $ uniform a b