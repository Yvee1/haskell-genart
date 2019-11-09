{-# LANGUAGE FlexibleInstances #-}
module Genart.Shapes.Circle where

import Genart.CairoHelpers
import Genart.Shapes.Types
circle :: Pt -> Double -> Circle
circle = Circle