module Util where

import Data.Text (Text)
import qualified System.Random as R
import System.Random.Shuffle (shuffle')


type Err = Text


-- Apply a function n times.
times :: Int -> (a -> a) -> a -> a
times n f x = (iterate f x) !! n


-- Convenient shuffle.
shuffle :: [a] -> Gen -> [a]
shuffle [] _       = []
shuffle xs (Gen g) = shuffle' xs (length xs) g


-- Special newtype-wrapped StdGen to ease equality checks.
newtype Gen = Gen R.StdGen

instance Eq Gen where
  _ == _ = True

instance Show Gen where
  show _ = "gen"

split :: Gen -> (Gen, Gen)
split (Gen g) = (Gen . fst $ R.split g, Gen . snd $ R.split g)

getGen :: IO Gen
getGen = fmap Gen R.getStdGen

mkGen :: Int -> Gen
mkGen n = Gen . R.mkStdGen $ n
