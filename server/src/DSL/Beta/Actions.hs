{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module DSL.Beta.Actions where

import Bounce (BounceState (..), CardBounce (..))
import CardAnim (Hurt (..), TimeModifier (..))
import Control.Monad (forM_, when)
import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import qualified DSL.Alpha as Alpha
import DSL.Beta.DSL (DSL (..), Program)
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import HandCard (HandCard, isRevealed)
import Life (Life)
import Model (maxHandLength)
import Player (WhichPlayer (..), other)
import qualified Stack
import StackCard (StackCard (..))
import Transmutation (Transmutation (..))
import Util (randomChoice, shuffle, split)
import Wheel (Wheel (..))
import qualified Wheel
import Prelude hiding (null)

makeFree ''DSL

transmute :: (Int -> StackCard -> Maybe Transmutation) -> Program ()
transmute f = do
  stack <- getStack
  transmute' $ Stack.diasporaMap f stack

bounce :: (Int -> StackCard -> Bool) -> TimeModifier -> Program ()
bounce f t = do
  bounces <- getBounces f
  bounce' bounces t

getBounces :: (Int -> StackCard -> Bool) -> Program (Wheel (Maybe CardBounce))
getBounces f = do
  stack <- getStack
  handALen <- length <$> getHand PlayerA
  handBLen <- length <$> getHand PlayerB
  let startState =
        ( BounceState
            { stackIndex = 0,
              handAIndex = handALen,
              handBIndex = handBLen,
              bounces = Wheel.init $ const Nothing
            }
        )
  let BounceState {bounces} = foldl' reduce startState stack
  return bounces
  where
    next :: BounceState -> BounceState
    next state@(BounceState {stackIndex, bounces}) =
      ( state
          { stackIndex = stackIndex + 1,
            bounces = Wheel.back bounces
          }
      )
    reduce :: BounceState -> Maybe StackCard -> BounceState
    reduce state Nothing = next state
    reduce state@(BounceState {stackIndex, handAIndex, handBIndex, bounces}) (Just stackCard) =
      next $
        if f stackIndex stackCard
          then case stackcard_owner stackCard of
            PlayerA ->
              if handAIndex >= maxHandLength
                then (state {bounces = bounces {wheel_0 = Just BounceDiscard}})
                else
                  ( state
                      { handAIndex = handAIndex + 1,
                        bounces = bounces {wheel_0 = Just (BounceIndex stackIndex handAIndex)}
                      }
                  )
            PlayerB ->
              if handBIndex >= maxHandLength
                then (state {bounces = bounces {wheel_0 = Just BounceDiscard}})
                else
                  ( state
                      { handBIndex = handBIndex + 1,
                        bounces = bounces {wheel_0 = Just (BounceIndex stackIndex handBIndex)}
                      }
                  )
          else state

discardStack :: (Int -> StackCard -> Bool) -> Program ()
discardStack f = do
  discards <- getStackDiscards f
  discardStack' discards

getStackDiscards :: (Int -> StackCard -> Bool) -> Program (Wheel Bool)
getStackDiscards f = do
  stack <- getStack
  let discarder = \i c -> Just $ f i c
  return $ fromMaybe False <$> Stack.diasporaMap discarder stack

moveStack :: (Int -> StackCard -> Maybe Int) -> TimeModifier -> Program ()
moveStack f t = do
  stack <- getStack
  moveStack' (Stack.diasporaMap f stack) t

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
