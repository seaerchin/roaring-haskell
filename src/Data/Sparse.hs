module Data.Sparse where

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
