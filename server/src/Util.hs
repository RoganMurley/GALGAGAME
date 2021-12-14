module Util where

import Control.DeepSeq (NFData(..))
import Control.Monad (replicateM_)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Data.Text (Text)
import qualified Data.Text as T
import qualified System.Random as R
import System.Random.Shuffle (shuffle')


type Err = Text


-- Apply a function n times.
times :: Int -> (a -> a) -> a -> a
times n f x = (iterate f x) !! n


-- Perform a monadic action n times
many :: Monad m => Int -> m a -> m ()
many = replicateM_


manyIndexed :: Monad m => Int -> (Int -> m a) -> m ()
manyIndexed i f = if i > 0 then f i >> manyIndexed (i - 1) f else return ()


shuffle :: Gen -> [a] -> [a]
shuffle _ []       = []
shuffle (Gen g) xs = shuffle' xs (length xs) g


randomChoice :: Gen -> [a] -> a
randomChoice gen xs = head $ shuffle gen xs


deleteIndex :: Int -> [a] -> [a]
deleteIndex n xs =
  ys ++ tail zs
    where
      (ys, zs) = splitAt n xs


fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Unsafe fromRight unwrapping failure"


-- Special newtype-wrapped StdGen to ease equality checks.
newtype Gen = Gen R.StdGen
  deriving Show


instance NFData Gen where
  rnf _ = ()


instance Eq Gen where
  _ == _ = True


split :: Gen -> (Gen, Gen)
split (Gen g) = (Gen g1, Gen g2)
  where
    (g1, g2) = R.split g :: (R.StdGen, R.StdGen)


getGen :: IO Gen
getGen = Gen <$> R.newStdGen


mkGen :: Int -> Gen
mkGen n = Gen . R.mkStdGen $ n


genToSeed :: Gen -> String
genToSeed (Gen gen) = show gen


seedToGen :: String -> Gen
seedToGen seed = Gen $ read seed


random :: Gen -> Float
random (Gen gen) = fst $ R.random gen


infiniteGens :: Gen -> [Gen]
infiniteGens gen = gen : (infiniteGens . fst $ split gen)


modTVar :: TVar a -> (a -> a) -> STM ()
modTVar var f = do
  x <- readTVar var
  writeTVar var (f x)

{-# INLINE modTVar #-}


modReadTVar :: TVar a -> (a -> a) -> STM a
modReadTVar var f = do
  x <- readTVar var
  let a = f x in
    do
      writeTVar var a
      return a
{-# INLINE modReadTVar #-}


modReturnTVar :: TVar a -> (a -> (a, b)) -> STM b
modReturnTVar var f = do
  x <- readTVar var
  let (a, b) = f x in
    do
      writeTVar var a
      return b
{-# INLINE modReturnTVar #-}


-- Works like Text.breakOn, but drops the text that was broke on.
breakAt :: Text -> Text -> (Text, Text)
breakAt b t =
  let
    (x, y) = T.breakOn b t :: (Text, Text)
  in
    (x, T.drop (T.length b) y)


indexedFilter :: (Int -> a -> Bool) -> [a] -> [a]
indexedFilter f xs = fmap snd $ filter (uncurry f) (zip [0..] xs)


tupleMap2 :: (a -> b) -> (a, a) -> (b, b)
tupleMap2 f (x, y) = (f x, f y)


to2Tuple :: [a] -> (a, a)
to2Tuple (x : y : _) = (x, y)
to2Tuple _ = error "Not enough values in list to make 2-tuple"


to3Tuple :: [a] -> (a, a, a)
to3Tuple (x : y : z : _) = (x, y, z)
to3Tuple _ = error "Not enough values in list to make 3-tuple"
