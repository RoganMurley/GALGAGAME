{-# LANGUAGE FlexibleContexts #-}

module Util where

import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.DeepSeq (NFData (..))
import Control.Monad (replicateM_, when)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import Data.Text qualified as T
import System.Random qualified as R
import System.Random.Shuffle (shuffle')

type Err = Text

-- Apply a function n times.
times :: Int -> (a -> a) -> a -> a
times n f x = (iterate f x) !! n

-- Perform a monadic action n times
many :: (Monad m) => Int -> m a -> m ()
many = replicateM_

manyIndexed :: (Monad m) => Int -> (Int -> m a) -> m ()
manyIndexed i f = when (i > 0) $ f i >> manyIndexed (i - 1) f

shuffle :: Gen -> [a] -> [a]
shuffle _ [] = []
shuffle (Gen g) xs = shuffle' xs (length xs) g

randomChoice :: Gen -> [a] -> a
randomChoice gen xs =
  case shuffle gen xs of
    (y : _) -> y
    [] -> error "randomChoice: empty list"

randomBetween :: Gen -> Int -> Int -> Int
randomBetween (Gen g) low high = fst $ R.randomR (low, high) g

deleteIndex :: Int -> [a] -> [a]
deleteIndex n xs =
  ys ++ drop 1 zs
  where
    (ys, zs) = splitAt n xs

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Unsafe fromRight unwrapping failure"

-- Special newtype-wrapped StdGen to ease equality checks.
newtype Gen = Gen R.StdGen
  deriving (Show)

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
  let a = f x
   in do
        writeTVar var a
        return a
{-# INLINE modReadTVar #-}

modReturnTVar :: TVar a -> (a -> (a, b)) -> STM b
modReturnTVar var f = do
  x <- readTVar var
  let (a, b) = f x
   in do
        writeTVar var a
        return b
{-# INLINE modReturnTVar #-}

-- Works like Text.breakOn, but drops the text that was broke on.
breakAt :: Text -> Text -> (Text, Text)
breakAt b t =
  let (x, y) = T.breakOn b t :: (Text, Text)
   in (x, T.drop (T.length b) y)

indexedFilter :: (Int -> a -> Bool) -> [a] -> [a]
indexedFilter f xs = fmap snd $ filter (uncurry f) (zip [0 ..] xs)

tupleMap2 :: (a -> b) -> (a, a) -> (b, b)
tupleMap2 f (x, y) = (f x, f y)

to2Tuple :: [a] -> (a, a)
to2Tuple (x : y : _) = (x, y)
to2Tuple _ = error "Not enough values in list to make 2-tuple"

to3Tuple :: [a] -> (a, a, a)
to3Tuple (x : y : z : _) = (x, y, z)
to3Tuple _ = error "Not enough values in list to make 3-tuple"

xor :: Bool -> Bool -> Bool
xor a b = a /= b

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

forkDelay :: (MonadBaseControl IO m) => Int -> m () -> m ()
forkDelay delay action = do
  _ <- fork $ do
    threadDelay delay
    action
  return ()
