module Util where

import System.Random (StdGen)
import System.Random.Shuffle (shuffle')


-- Apply a function n times.
times :: Int -> (a -> a) -> a -> a
times n f x = (iterate f x) !! n

-- Convenient shuffle.
shuffle :: [a] -> (StdGen -> [a])
shuffle xs = shuffle' xs (length xs)
