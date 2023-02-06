{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module DSL.Beta.Actions where

import Bounce (BounceState (..), CardBounce (..))
import Card (Status (..), hasStatus)
import CardAnim (Hurt (..), TimeModifier (..))
import Control.Monad (forM_, when)
import Control.Monad.Freer.TH (makeEffect)
import qualified DSL.Alpha as Alpha
import DSL.Beta.DSL (DSL, Program)
import Data.Foldable (foldl', toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import HandCard (HandCard, isRevealed)
import Life (Life)
import Model (Model (model_misc), getNoDraws, maxHandLength)
import Player (WhichPlayer (..), other)
import Safe (headDef, lastDef)
import Stack (Stack)
import qualified Stack
import StackCard (StackCard (..), isOwner)
import Transmutation (Transmutation (..), removeTransmuteToSelf)
import Util (randomChoice, shuffle, split)
import Wheel (Wheel (..), indexWheel)
import qualified Wheel
import Prelude hiding (null)

makeEffect ''DSL

transmute :: (Int -> StackCard -> Maybe Transmutation) -> Program ()
transmute f = do
  stack <- getStack
  let transmutations = removeTransmuteToSelf <$> Stack.diasporaMap f stack
  transmute' transmutations
  discardStack' ((&&) <$> isFragile stack <*> (isJust <$> transmutations))

transmuteActive :: (StackCard -> Maybe StackCard) -> Program ()
transmuteActive f = do
  stack <- getStack
  let mActiveCard = wheel_0 stack
  case mActiveCard of
    Just activeCard ->
      case f activeCard of
        Just finalCard -> do
          transmuteActive' $ Transmutation activeCard finalCard
          when (hasFragileStatus activeCard) $ do
            discardStack' (Wheel.init (0 ==))
            raw $ Alpha.setHold False
        Nothing ->
          return ()
    Nothing ->
      return ()

draw :: WhichPlayer -> WhichPlayer -> TimeModifier -> Program ()
draw w wd t = do
  noDraws <- getNoDraws wd . model_misc <$> getModel
  when (noDraws > 10) (discardHand w (\_ _ -> True))
  draw' w wd t

bounce :: (Int -> StackCard -> Bool) -> TimeModifier -> Program ()
bounce f t = do
  stack <- getStack
  bounces <- getBounces f
  bounce' bounces t
  let paTargets = catMaybes . toList $ playerBounce PlayerA <$> bounces <*> stack
  discardHand PlayerA (\i _ -> i `elem` paTargets)
  let pbTargets = catMaybes . toList $ playerBounce PlayerB <$> bounces <*> stack
  discardHand PlayerB (\i _ -> i `elem` pbTargets)
  where
    playerBounce :: WhichPlayer -> Maybe CardBounce -> Maybe StackCard -> Maybe Int
    playerBounce w (Just (BounceIndex _ targetIndex)) (Just sc) =
      if isOwner w sc && hasStatus StatusFragile (stackcard_card sc)
        then Just targetIndex
        else Nothing
    playerBounce _ _ _ = Nothing

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
    next state@BounceState {stackIndex, bounces} =
      ( state
          { stackIndex = stackIndex + 1,
            bounces = Wheel.back bounces
          }
      )
    reduce :: BounceState -> Maybe StackCard -> BounceState
    reduce state Nothing = next state
    reduce state@BounceState {stackIndex, handAIndex, handBIndex, bounces} (Just stackCard) =
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
  let moves = removeMoveToSelf <$> indexWheel <*> Stack.diasporaMap f stack
  moveStack' moves t
  movedStack <- getStack
  let targets = foldl' reduce Set.empty moves
  discardStack' ((&&) <$> isFragile movedStack <*> ((`Set.member` targets) <$> indexWheel))
  where
    reduce :: Set Int -> Maybe Int -> Set Int
    reduce set (Just i) = Set.insert (i `mod` 12) set
    reduce set Nothing = set

    removeMoveToSelf :: Int -> Maybe Int -> Maybe Int
    removeMoveToSelf origin (Just target) =
      if origin == target then Nothing else Just target
    removeMoveToSelf _ Nothing = Nothing

isFragile :: Stack -> Wheel Bool
isFragile =
  fmap
    (maybe False hasFragileStatus)

hasFragileStatus :: StackCard -> Bool
hasFragileStatus sc = hasStatus StatusFragile $ stackcard_card sc

lifesteal :: Life -> WhichPlayer -> Program ()
lifesteal d w = do
  initial <- getLife w
  hurt d w Slash
  final <- getLife w
  let h = initial - final
  heal h (other w)

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
  forM_ [0 .. n] $ const shuffleMovingEveryCard
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
          let time = (1 + lastDef 0 diasporaIs - headDef 0 diasporaIs) * 20
          refreshGen
          if anyElemsSame shuffledIs diasporaIs
            then shuffleMovingEveryCard
            else moveStack (\i _ -> Map.lookup i diasporaMap) (TimeModifierOutQuad $ fromIntegral time)
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
