module Data.Dense where

import Data.Sparse (Sparse)
import Data.Vector (Vector, (//))
import qualified Data.Vector as V

-- This file describes a way of storing densely populated data using bitmaps.
-- A bitmap is simply a vector of bits.
-- The bitmap has an address space of 16 bits and size 2^16 bits (by definition)

type Bit = Bool

newtype Dense = Bitmap (Vector Bit) deriving (Show, Eq, Ord)

emptyVec :: Vector Bit
emptyVec = V.replicate (2 ^ 16) False

empty :: Dense
empty = Bitmap emptyVec

toList :: Dense -> [Bit]
toList (Bitmap x) = V.toList x

length :: Dense -> Int
length (Bitmap x) = V.length x