{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module DSL.Alpha.Actions where

import Bounce (CardBounce (..))
import Card (Card)
import {-# SOURCE #-} Cards (getEndCard)
import Control.Applicative ((<|>))
import Control.Monad.Freer.TH (makeEffect)
import DSL.Alpha.DSL (DSL, Program)
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import HandCard (HandCard (..))
import HandCard qualified
import Life (Life)
import Model (Deck, Hand, Misc (..), Passes (..), Turn, getNoDraws, incrNoDraws, maxHandLength)
import Player (WhichPlayer (..), other)
import Safe (headMay, tailSafe)
import Stack (Stack)
import Stack qualified
import StackCard (StackCard (..), isOwner)
import Transmutation (Transmutation (..))
import Util (deleteIndex, indexedFilter)
import Wheel (Wheel (..))

makeEffect ''DSL

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

modMisc :: (Misc -> Misc) -> Program ()
modMisc f = getMisc >>= (setMisc . f)

modPasses :: (Passes -> Passes) -> Program ()
modPasses f = getPasses >>= (setPasses . f)

modStackActive :: (StackCard -> StackCard) -> Program ()
modStackActive f = do
  stack <- getStack
  case wheel_0 stack of
    Just c ->
      setStack $ stack {wheel_0 = Just (f c)}
    Nothing ->
      return ()

modStackHead :: (StackCard -> StackCard) -> Program ()
modStackHead f = do
  stack <- getStack
  case wheel_1 stack of
    Just c ->
      setStack $ stack {wheel_1 = Just (f c)}
    Nothing ->
      return ()

hurt :: Life -> WhichPlayer -> Program ()
hurt dmg w = modLife w (-dmg +)

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
  modStack $ (\stack -> stack {wheel_0 = Just (StackCard w c)})

incPasses :: Passes -> Passes
incPasses NoPass = OnePass
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
      addToHand w card
    Nothing -> do
      noDraws <- getNoDraws w <$> getMisc
      addToHand w (KnownHandCard (getEndCard noDraws))
      modMisc (incrNoDraws w)

transmute :: Wheel (Maybe Transmutation) -> Program ()
transmute transmutations =
  let combiner :: Maybe Transmutation -> Maybe StackCard -> Maybe StackCard
      combiner (Just (Transmutation _ finalStackCard)) (Just _) = Just finalStackCard
      combiner _ stackCard = stackCard
   in do
        stack <- getStack
        setStack (combiner <$> transmutations <*> stack)

transmuteActive :: Transmutation -> Program ()
transmuteActive (Transmutation _ finalCard) = do
  modStack (\stack -> stack {wheel_0 = Just finalCard})
  setHold True

bounce :: Wheel (Maybe CardBounce) -> Program ()
bounce bounces = do
  stack <- getStack
  -- Remove bounced/discarded cards
  setStack $ remove <$> bounces <*> stack
  -- PlayerA
  let paBounceCards = catMaybes . toList $ playerBounce PlayerA <$> bounces <*> stack
  modHand PlayerA $ \h -> h ++ (KnownHandCard . stackcard_card <$> paBounceCards)
  -- PlayerB
  let pbBounceCards = catMaybes . toList $ playerBounce PlayerB <$> bounces <*> stack
  modHand PlayerB $ \h -> h ++ (KnownHandCard . stackcard_card <$> pbBounceCards)
  where
    remove :: Maybe CardBounce -> Maybe StackCard -> Maybe StackCard
    remove (Just _) _ = Nothing
    remove Nothing mStackCard = mStackCard

    playerBounce :: WhichPlayer -> Maybe CardBounce -> Maybe StackCard -> Maybe StackCard
    playerBounce w (Just (BounceIndex _ _)) (Just sc) = if isOwner w sc then Just sc else Nothing
    playerBounce _ _ _ = Nothing

bounceDeck :: Wheel Bool -> Program ()
bounceDeck bounces = do
  stack <- getStack
  -- Remove bounced/discarded cards
  setStack $ remove <$> bounces <*> stack
  -- PlayerA
  let paBounceCards = catMaybes . toList $ playerBounce PlayerA <$> bounces <*> stack
  modDeck PlayerA $ \d -> (KnownHandCard . stackcard_card <$> paBounceCards) ++ d
  -- PlayerB
  let pbBounceCards = catMaybes . toList $ playerBounce PlayerB <$> bounces <*> stack
  modDeck PlayerB $ \d -> (KnownHandCard . stackcard_card <$> pbBounceCards) ++ d
  where
    remove :: Bool -> Maybe StackCard -> Maybe StackCard
    remove True _ = Nothing
    remove False mStackCard = mStackCard

    playerBounce :: WhichPlayer -> Bool -> Maybe StackCard -> Maybe StackCard
    playerBounce w True (Just sc) = if isOwner w sc then Just sc else Nothing
    playerBounce _ _ _ = Nothing

moveStack :: Wheel (Maybe Int) -> Program ()
moveStack moves =
  let -- Stack with moved cards at their targets.
      targetReduce :: (Maybe Int, Maybe StackCard) -> Stack -> Stack
      targetReduce (Just i, mStackCard) stack = Stack.set stack (i `mod` 12) mStackCard
      targetReduce (Nothing, _) stack = stack
      invertMaybe :: Maybe a -> Maybe ()
      invertMaybe (Just _) = Nothing
      invertMaybe Nothing = Just ()
   in do
        stack <- getStack
        -- Stack with moved cards at their targets.
        let targets = foldr targetReduce Stack.init ((,) <$> moves <*> stack) :: Stack
        -- Stack with static cards only.
        let statics = (*>) <$> (invertMaybe <$> moves) <*> stack
        -- Stack with cards at their final positions.
        let newStack = (<|>) <$> targets <*> statics :: Stack
        setStack newStack

discardStack :: Wheel Bool -> Program ()
discardStack discards =
  let f :: Bool -> Maybe StackCard -> Maybe StackCard
      f False mStackCard = mStackCard
      f True _ = Nothing
   in modStack $ \stack -> f <$> discards <*> stack

-- modStack $ Stack.diasporaFilter (\i c -> not $ f i c)

discardHand :: WhichPlayer -> (Int -> Card -> Bool) -> Program ()
discardHand w f = modHand w $ indexedFilter (\i c -> not $ f i (HandCard.anyCard c))

reveal :: WhichPlayer -> (Int -> Card -> Bool) -> Program ()
reveal w f = modHand w $ \h -> fmap revealer $ zip [0 ..] h
  where
    revealer :: (Int, HandCard) -> HandCard
    revealer (i, HandCard c) = if f i c then KnownHandCard c else HandCard c
    revealer (_, KnownHandCard c) = KnownHandCard c

revealDeck :: WhichPlayer -> Program ()
revealDeck w = modDeck w revealCard
  where
    revealCard :: Deck -> Deck
    revealCard (card : deck) =
      if HandCard.isRevealed card
        then card : revealCard deck
        else HandCard.reveal card : deck
    revealCard deck = deck

rotate :: Program ()
rotate = do
  modStack Stack.rotate
  modRot (\x -> x - 1)

windup :: Program ()
windup = do
  modStack Stack.windup
  modRot (1 +)

mill :: WhichPlayer -> Program ()
mill w = modDeck w tailSafe
