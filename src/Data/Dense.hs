module Data.Dense where

import Data.Sparse (Sparse)
import Data.Vector (Vector, (!), (//))
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

add :: Dense -> Int -> Dense
add (Bitmap d) = Bitmap . (d //) . flip zip (repeat True) . repeat

(+||+) :: Dense -> [Int] -> Dense
a +||+ ls = foldr (flip add) a ls

-- NOTE: The paper actually uses bit operations, which is a better way of doing things
-- This is done solely because it's more idiomatic in haskell ._.
-- Technically this could be written as Dense (zipWith or a b)
combineDense :: Dense -> Dense -> Dense
combineDense first = (+||+) first . map fst . filter snd . zip [0 ..] . toList

intersectDense :: Dense -> Dense -> Dense
intersectDense (Bitmap first) (Bitmap second) = Bitmap $ V.zipWith (&&) first second

-- Note that although we use the unsafe index operator
-- This operation is guaranteed to be safe
-- This is because all sparse arrays are guaranteed to have cardinality < 4096
intersect :: Dense -> [Int] -> Dense
intersect bm@(Bitmap d) ls = Bitmap $ d // bits ls
  where
    bits = filter (not . snd) . map (\idx -> (idx, d ! idx))