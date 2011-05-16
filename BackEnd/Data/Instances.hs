{-# LANGUAGE FlexibleContexts #-}
module Data.Instances () where

import Control.DeepSeq
import Data.Array.IArray
import Data.Array.Unboxed


instance (NFData i, NFData v, Ix i, IArray UArray v)
         => NFData (UArray i v) where
