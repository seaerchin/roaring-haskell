module Data.Core where

import Data.Dense (Dense (Bitmap), emptyVec, toList, (+||+))
import qualified Data.Dense as D
import Data.Run (Run)
import Data.Sparse (Sparse)
import qualified Data.Sparse as S
import Data.Vector (Vector, (//))
import qualified Data.Vector as V

-- Roaring bitmaps work through segregating the data into a 2-tiered space.
-- The top most level is chunked into 2^16 bits, which represent the most significant 16 digits of the data
data Inner = Sparse Sparse | Run Run | Dense Dense deriving (Show, Eq, Ord)

-- Roaring wraps the ranges of the underlying data type
data Roaring a = Roar Int Inner deriving (Show, Eq, Ord)

-- Because roaring bitmaps have 2 monoidal operations
-- 1. Intersection
-- 2. Union
-- There are 2 newtypes defined here as wrappers to identify which the programmer wants.
-- There is also no explicit monoidal operation defined on the base Inner type for this exact reason.
newtype Union = Union {getInnerFromUnion :: Inner}

newtype Intersection = Intersection {getInnerFromIntersect :: Inner}

-- Union of two different bitmaps
-- Simply take everything
instance Semigroup Union where
  -- TODO: self union
  Union (Dense a) <> Union (Dense b) = undefined
  Union (Sparse a) <> Union (Sparse b) = undefined
  Union (Dense a) <> Union (Sparse b) = (Union . Dense) (a +||+ b)
  Union (Sparse b) <> Union (Dense a) = (Union . Dense) (a +||+ b)

-- Inserts an integer into the container
-- O(n) time - Haskell lists are singly linked lists so indexing takes O(n).
-- Thus, this is written using a linear search
insert :: Int -> Inner -> Inner
insert n (Sparse x) = convert . Sparse $ S.insert n x
insert n (Dense x) = convert . Dense $ D.add x n

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
