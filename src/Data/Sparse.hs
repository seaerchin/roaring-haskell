module Data.Sparse where

import Data.Map (fromList, member)

-- This file describes a way of storing sparsely populated data using a dynamic array (in here, a list).

type Sparse = [Int]

(+.+) :: Sparse -> Sparse -> Sparse
a +.+ b = foldr insert a b

-- Prerequisite: the container is already sorted
-- Inserts an integer into the container.
-- O(n) time - Haskell lists are singly linked lists so indexing takes O(n).
-- Thus, this is written using a linear search
insert :: Int -> Sparse -> Sparse
insert n x = prefix ++ [n] ++ suffix
  where
    prefix = takeWhile (< n) x
    suffix = dropWhile (< n) x

combineSparse :: Sparse -> Sparse -> Sparse
combineSparse a b = go a b []
  where
    go [] a sorted = reverse sorted ++ a
    go a [] sorted = reverse sorted ++ a
    go xss@(x : xs) yss@(y : ys) sorted
      | x >= y = go xss ys (y : sorted)
      | otherwise = go xs yss (x : sorted)

-- The paper uses a galloping intersection to calculate intersection of two arrays
-- For simplicity, this is written here in O(n lg n) time using a two-step process
-- 1. Convert the first array into a hashmap
-- 2. Iterate recursively over the second, checking if values are within the hashmap
-- No need to sort because the second array is already sorted -> appending in sorted order means output is sorted
intersectSparse :: Sparse -> Sparse -> Sparse
intersectSparse a = reverse . foldr f []
  where
    hm = fromList (zip a (repeat True))
    f element prev = if element `member` hm then element : prev else prev