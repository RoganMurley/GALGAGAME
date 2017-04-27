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


-- Delete index.
deleteIndex :: Int -> [a] -> [a]
deleteIndex n xs =
  ys ++ (tail zs)
    where
      (ys, zs) = splitAt n xs


-- Unsafe fromRight.
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Unsafe fromRight unwrapping failure"


-- Special newtype-wrapped StdGen to ease equality checks.
newtype Gen = Gen R.StdGen

instance Eq Gen where
  _ == _ = True

instance Show Gen where
  show _ = "gen"

split :: Gen -> (Gen, Gen)
split (Gen g) = (Gen g1, Gen g2)
  where
    (g1, g2) = R.split g :: (R.StdGen, R.StdGen)

getGen :: IO Gen
getGen = fmap Gen R.getStdGen

mkGen :: Int -> Gen
mkGen n = Gen . R.mkStdGen $ n
