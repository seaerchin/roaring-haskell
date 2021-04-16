module Data.Core where

import Data.Dense (Dense (Bitmap), combineDense, emptyVec, intersect, toList, (+||+))
import qualified Data.Dense as D
import Data.Run (Run)
import Data.Sparse (Sparse, combineSparse)
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
newtype Union = Union {getInnerFromUnion :: Inner} deriving (Show, Eq)

newtype Intersection = Intersection {getInnerFromIntersect :: Inner} deriving (Show, Eq)

instance Semigroup Union where
  Union (Dense a) <> Union (Dense b) = (Union . Dense) $ combineDense a b
  -- NOTE: Type conversion is done here to ensure that an appropriate container is chosen
  Union (Sparse a) <> Union (Sparse b) = Union . convert . Sparse $ combineSparse a b
  Union (Dense a) <> Union (Sparse b) = (Union . Dense) (a +||+ b)
  Union (Sparse b) <> Union (Dense a) = (Union . Dense) (a +||+ b)

instance Semigroup Intersection where
  Intersection (Dense a) <> Intersection (Dense b) = (Intersection . convert . Dense) $ combineDense a b
  Intersection (Sparse a) <> Intersection (Sparse b) = (Intersection . Sparse) $ combineSparse a b
  Intersection (Dense a) <> Intersection (Sparse b) = (Intersection . convert . Dense) $ intersect a b
  Intersection (Sparse a) <> Intersection (Dense b) = (Intersection . convert . Dense) $ intersect b a

instance Monoid Union where
  mempty = (Union . Dense) D.empty

instance Monoid Intersection where
  mempty = (Intersection . Sparse) []

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
  | otherwise = n
convert n@(Run x) = n

fromSparse :: Sparse -> Dense
fromSparse ls = Bitmap (emptyVec // zipped)
  where
    zipped = zip ls (repeat True)

-- zip together with indexes
-- filter by true
-- change the type to list
fromDense :: Dense -> Sparse
fromDense = map fst . filter snd . zip [0 ..] . toList

main :: IO ()
main =
  do
    let a = Sparse [1, 2, 3]
    let y = D.empty
    let z = y +||+ [4094, 4095] +||+ [1, 2, 3]
    -- let z = y +||+ [0, 1]
    let helper = map fst . filter snd . zip [0 ..] . D.toList
    let intersected = Intersection a <> Intersection (Dense z)
    print intersected
    let b = Sparse [2, 3, 4, 5, 13]
    let union = Union (Dense z) <> Union b
    print b
    print z
    print union
    print (helper z)