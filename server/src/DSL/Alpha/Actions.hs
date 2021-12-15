{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module DSL.Alpha.Actions where

import Card (Card)
import Control.Applicative ((<|>))
import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import Data.List (partition)
import DSL.Alpha.DSL (DSL(..), Program)
import HandCard (HandCard(..), anyCard)
import Player (WhichPlayer(..), other)
import Life (Life)
import Model (Deck, Hand, Passes(..), Turn, maxHandLength)
import Safe (headMay, tailSafe)
import Stack (Stack)
import StackCard (StackCard(..), isOwner)
import Transmutation (Transmutation(..))
import Util (deleteIndex, indexedFilter)
import Wheel (Wheel(..))

import qualified Stack

import {-# SOURCE #-} Cards (strangeEnd)


makeFree ''DSL


modifier :: (WhichPlayer -> Program a) -> (WhichPlayer -> a -> Program ()) -> WhichPlayer -> (a -> a) -> Program ()
modifier getter setter w f = do
  x <- getter w
  setter w (f x)


modLife :: WhichPlayer -> (Life -> Life) -> Program ()
modLife = modifier getLife setLife

modHand :: WhichPlayer -> (Hand -> Hand) -> Program ()
modHand = modifier getHand setHand


modDeck :: WhichPlayer -> (Deck -> Deck) -> Program ()
modDeck = modifier getDeck setDeck


modStack :: (Stack -> Stack) -> Program ()
modStack f = getStack >>= (setStack . f)


modTurn :: (Turn -> Turn) -> Program ()
modTurn f = getTurn >>= (setTurn . f)


modRot :: (Int -> Int) -> Program ()
modRot f = getRot >>= (setRot . f)


modPasses :: (Passes -> Passes) -> Program ()
modPasses f = getPasses >>= (setPasses . f)


modStackHead :: (StackCard -> StackCard) -> Program ()
modStackHead f = do
  stack <- getStack
  case wheel_1 stack of
    Just c ->
      setStack $ stack { wheel_1 = Just (f c) }
    Nothing ->
      return ()


hurt :: Life -> WhichPlayer -> Program ()
hurt dmg w = modLife w (-dmg+)


heal :: Life -> WhichPlayer -> Program ()
heal mag w = do
  maxLife <- getMaxLife w
  modLife w (\l -> min maxLife (l + mag))


lifesteal :: Life -> WhichPlayer -> Program ()
lifesteal dmg w = do
  hurt dmg w
  heal dmg (other w)


play :: WhichPlayer -> Card -> Int -> Program ()
play w c i = do
  swapTurn
  resetPasses
  modHand w $ deleteIndex i
  modStack $ (\stack -> stack { wheel_0 = Just (StackCard w c) })


incPasses :: Passes -> Passes
incPasses NoPass  = OnePass
incPasses OnePass = NoPass


resetPasses :: Program ()
resetPasses = setPasses NoPass


swapTurn :: Program ()
swapTurn = do
  modTurn other
  modPasses incPasses


addToHand :: WhichPlayer -> HandCard -> Program ()
addToHand w c = modHand w (\h -> h ++ [c])


handFull :: WhichPlayer -> Program Bool
handFull w = do
  handLength <- length <$> getHand w
  return $ handLength >= maxHandLength


draw :: WhichPlayer -> WhichPlayer -> Program ()
draw w d = do
  deck <- getDeck d
  case headMay deck of
    Just card -> do
      modDeck d tailSafe
      addToHand w (HandCard card)
    Nothing ->
      addToHand w (KnownHandCard strangeEnd)


transmute :: (Int -> StackCard -> Maybe Transmutation) -> Program ()
transmute f =
  let
    combiner :: Maybe Transmutation -> Maybe StackCard -> Maybe StackCard
    combiner (Just (Transmutation _ finalStackCard)) (Just _)  = Just finalStackCard
    combiner _                                       stackCard = stackCard
  in
  do
    stack <- getStack
    let transmutations = Stack.diasporaMap f stack
    setStack (combiner <$> transmutations <*> stack)


transmuteActive :: (StackCard -> Maybe StackCard) -> Program ()
transmuteActive f =
  do
    stack <- getStack
    case wheel_0 stack of
      Just activeCard ->
        case f activeCard of
          Just finalStackCard -> do
            setStack (stack { wheel_0 = Just finalStackCard })
            setHold True
          Nothing ->
            return ()
      Nothing ->
        return ()


bounce :: (Int -> StackCard -> Bool) -> Program ()
bounce f = do
  diaspora <- Stack.diasporaFromStack <$> getStack
  let stackCards = fmap snd diaspora :: [StackCard]
  modStack $ Stack.diasporaFilter (\i c -> not $ f i c)
  let bouncing = indexedFilter f stackCards
  let (paBouncing, pbBouncing) = partition (isOwner PlayerA) bouncing
  modHand PlayerA $ \h -> h ++ (KnownHandCard . stackcard_card <$> paBouncing)
  modHand PlayerB $ \h -> h ++ (KnownHandCard . stackcard_card <$> pbBouncing)


moveStack :: (Int -> StackCard -> Maybe Int) -> Program ()
moveStack f =
  let
    -- Stack with moved cards at their targets.
    targetReduce :: (Maybe Int, Maybe StackCard) -> Stack -> Stack
    targetReduce (Just i, mStackCard) stack = Stack.set stack i mStackCard
    targetReduce (Nothing, _) stack         = stack
    invertMaybe :: Maybe a -> Maybe ()
    invertMaybe (Just _) = Nothing
    invertMaybe Nothing  = Just ()
  in
    do
      stack <- getStack
      let moves = Stack.diasporaMap f stack
      -- Stack with moved cards at their targets.
      let targets = foldr targetReduce Stack.init ((,) <$> moves <*> stack) :: Stack
      -- Stack with static cards only.
      let statics = (invertMaybe <$> moves) *> stack
      -- Stack with cards at their final positions.
      let newStack = (<|>) <$> targets <*> statics :: Stack
      setStack newStack


discardStack :: (Int -> StackCard -> Bool) -> Program ()
discardStack f = modStack $ Stack.diasporaFilter (\i c -> not $ f i c)


discardHand :: WhichPlayer -> (Int -> Card -> Bool) -> Program ()
discardHand w f = modHand w $ indexedFilter (\i c -> not $ f i (anyCard c))


rotate :: Program ()
rotate = do
  modStack Stack.rotate
  modRot (\x -> x - 1)


windup :: Program ()
windup = do
  modStack Stack.windup
  modRot ((+) 1)


mill :: WhichPlayer -> Program ()
mill w = modDeck w tailSafe
