module Data.Run where

-- This file describes a way of storing densely populated and continuous data using a run.
-- A run is essentially 2 things - (lower, limit).
-- The lowerbound is simply lower and the upperbound is lower + limit.
-- Hence, the run stores [lower, upper] items in very little space

data Run = Run {lower :: Int, size :: Int} deriving (Show, Eq, Ord)