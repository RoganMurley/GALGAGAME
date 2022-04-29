{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module DSL.Beta.Actions where

import CardAnim (Hurt (..), TimeModifier (..))
import Control.Monad (forM_, when)
import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import qualified DSL.Alpha as Alpha
import DSL.Beta.DSL (DSL (..), Program)
import Data.Map (Map)
import qualified Data.Map as Map
import HandCard (HandCard, isRevealed)
import Life (Life)
import Player (WhichPlayer (..), other)
import qualified Stack
import StackCard (StackCard (..))
import Transmutation (Transmutation (..))
import Util (randomChoice, shuffle, split)
import Prelude hiding (null)

makeFree ''DSL

lifesteal :: Life -> WhichPlayer -> Program ()
lifesteal d w = do
  hurt d w Slash
  heal d (other w)

refreshGen :: Program ()
refreshGen =
  raw $ do
    gen <- Alpha.getGen
    let (newGen, _) = split gen
    Alpha.setGen newGen

transmuteHead :: (StackCard -> StackCard) -> Program ()
transmuteHead f = transmute transmuter
  where
    transmuter :: Int -> StackCard -> Maybe Transmutation
    transmuter 1 sc = Just $ Transmutation sc (f sc)
    transmuter _ _ = Nothing

confound :: Program ()
confound = do
  initialGen <- getGen
  let n = randomChoice initialGen [1 .. 4] :: Int
  forM_ [1 .. n] $ const shuffleMovingEveryCard
  where
    -- Shuffle, guaranteeing that every card is moved.
    -- Not truly random, but good enough because we do it a random
    -- number of times.
    shuffleMovingEveryCard :: Program ()
    shuffleMovingEveryCard = do
      gen <- getGen
      stack <- getStack
      let diasporaIs = drop 1 $ fst <$> Stack.diasporaFromStack stack :: [Int]
      let len = length diasporaIs
      when (len > 1) $
        do
          let shuffledIs = shuffle gen diasporaIs :: [Int]
          let diasporaMap = Map.fromList $ zip diasporaIs shuffledIs :: Map Int Int
          refreshGen
          if anyElemsSame shuffledIs diasporaIs
            then shuffleMovingEveryCard
            else moveStack (\i _ -> Map.lookup i diasporaMap) (TimeModifierOutQuad (fromIntegral len * 20))
    -- Check if any list elements are at the same position
    anyElemsSame :: Eq a => [a] -> [a] -> Bool
    anyElemsSame xs ys = foldr (\(x, y) prev -> x == y || prev) False $ zip xs ys

reversal :: Program ()
reversal = do
  diaspora <- Stack.diasporaFromStack <$> getStack
  let diasporaIs = drop 1 $ fst <$> diaspora :: [Int]
  let reversedIs = reverse diasporaIs :: [Int]
  let diasporaMap = Map.fromList $ zip diasporaIs reversedIs :: Map Int Int
  moveStack (\i _ -> Map.lookup i diasporaMap) (TimeModifierOutQuint 500)

revealRandomCard :: WhichPlayer -> Program ()
revealRandomCard w = do
  hand <- getHand w
  let indexed = zip [0 ..] hand :: [(Int, HandCard)]
  let hidden = filter (\(_, c) -> not (isRevealed c)) indexed :: [(Int, HandCard)]
  when (length hidden > 0) $ do
    g <- getGen
    let targetIndex = fst $ randomChoice g hidden
    reveal w (\i _ -> i == targetIndex)
