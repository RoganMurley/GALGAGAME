module Util where

import System.Random.Shuffle (shuffle')


-- Apply a function n times.
times :: Int -> (a -> a) -> a -> a
times n f x = (iterate f x) !! n

-- Convenient shuffle.
shuffle :: [a] -> b
shuffle xs = shuffle xs (length xs)
