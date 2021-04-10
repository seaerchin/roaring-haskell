module Data.Core where

import Data.Dense (Dense (Bitmap), emptyVec, toList)
import qualified Data.Dense as D
import Data.Run (Run)
import Data.Sparse (Sparse)
import Data.Vector (Vector, (//))
import qualified Data.Vector as V

-- Roaring bitmaps work through segregating the data into a 2-tiered space.
-- The top most level is chunked into 2^16 bits, which represent the most significant 16 digits of the data
data Inner = Sparse Sparse | Run Run | Dense Dense deriving (Show, Eq, Ord)

-- Roaring wraps the ranges of the underlying data type
data Roaring a = Roar Int Inner deriving (Show, Eq, Ord)

-- WIP
instance Semigroup Inner where
  a <> b = a

instance Monoid Inner where
  mempty = Sparse []

-- Inserts an integer into the container
-- O(n) time - Haskell lists are singly linked lists so indexing takes O(n)
-- Thus, this is written using a linear search
insert :: Int -> Inner -> Inner
insert n (Sparse x) = Sparse (prefix ++ [n] ++ suffix)
  where
    prefix = takeWhile (< n) x
    suffix = dropWhile (< n) x

-- Conversions from differing dtypes
convert :: Inner -> Inner
-- Convert only if memory compression
convert n@(Sparse x)
  | length x >= 4096 = Dense $ fromSparse x
  | otherwise = n
convert n@(Dense x)
  | D.length x < 4096 = Sparse $ fromDense x

fromSparse :: Sparse -> Dense
fromSparse ls = Bitmap (emptyVec // zipped)
  where
    zipped = zip ls (repeat True)

-- zip together with indexes
-- filter by true
-- change the type to list
fromDense :: Dense -> Sparse
fromDense = map fst . filter snd . zip [0 ..] . toList
