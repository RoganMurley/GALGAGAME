{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module DSL.Alpha.Actions where

import Card (Card)
import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import Data.List (partition)
import DSL.Alpha.DSL (DSL(..), Program)
import Player (WhichPlayer(..), other)
import Life (Life)
import Model (Deck, Hand, Limbo, Passes(..), Stack, Turn, maxHandLength)
import Safe (headMay, tailSafe)
import StackCard (StackCard(StackCard, stackcard_card), isOwner)
import Transmutation (Transmutation(..))
import Util (deleteIndex, indexedFilter, shuffle)

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


modLimbo :: (Limbo -> Limbo) -> Program ()
modLimbo f = getLimbo >>= (setLimbo . f)


modStackAll :: (StackCard -> StackCard) -> Program ()
modStackAll f = modStack $ fmap f


modTurn :: (Turn -> Turn) -> Program ()
modTurn f = getTurn >>= (setTurn . f)


modRot :: (Int -> Int) -> Program ()
modRot f = getRot >>= (setRot . f)


modPasses :: (Passes -> Passes) -> Program ()
modPasses f = getPasses >>= (setPasses . f)


modStackHead :: (StackCard -> StackCard) -> Program ()
modStackHead f = do
  s <- getStack
  case headMay s of
    Just c ->
      setStack $ f c : (tailSafe s)
    Nothing ->
      return ()



hurt :: Life -> WhichPlayer -> Program ()
hurt dmg w = modLife w (-dmg+)


heal :: Life -> WhichPlayer -> Program ()
heal mag w = modLife w (+mag)


lifesteal :: Life -> WhichPlayer -> Program ()
lifesteal dmg w = do
  hurt dmg w
  heal dmg (other w)


play :: WhichPlayer -> Card -> Int -> Program ()
play w c i = do
  swapTurn
  resetPasses
  modHand w $ deleteIndex i
  modStack $ (:) (StackCard w c)


incPasses :: Passes -> Passes
incPasses NoPass  = OnePass
incPasses OnePass = NoPass


resetPasses :: Program ()
resetPasses = setPasses NoPass


swapTurn :: Program ()
swapTurn = do
  modTurn other
  modPasses incPasses

addToHand :: WhichPlayer -> Card -> Program ()
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
    Nothing ->
      addToHand w strangeEnd


transmute :: ((Int, StackCard) -> Transmutation) -> Program ()
transmute f =
  let
    combiner :: Transmutation -> StackCard -> StackCard
    combiner (Transmutation _ finalStackCard) _         = finalStackCard
    combiner NoTransmutation                  stackCard = stackCard
  in
  do
    stack <- getStack
    let transmutations = f <$> zip [0..] stack
    let newStack = uncurry combiner <$> zip transmutations stack
    setStack newStack


bounce :: (StackCard -> Bool) -> Program ()
bounce f = do
  stack <- getStack
  let (bouncing, staying) = partition f stack
  setStack staying
  let (paBouncing, pbBouncing) = partition (isOwner PlayerA) bouncing
  modHand PlayerA $ \h -> h ++ (stackcard_card <$> paBouncing)
  modHand PlayerB $ \h -> h ++ (stackcard_card <$> pbBouncing)


discardStack :: ((Int, StackCard) -> Bool) -> Program ()
discardStack f = modStack $ indexedFilter (not . f)


discardHand :: WhichPlayer -> ((Int, Card) -> Bool) -> Program ()
discardHand w f = modHand w $ indexedFilter (not . f)


confound :: Program ()
confound = do
  gen <- getGen
  modStack $ shuffle gen


rotate :: Program ()
rotate = do
  modStack tailSafe
  modRot (\x -> x - 1)


windup :: Program ()
windup = do
  modRot ((+) 1)


limbo :: ((Int, StackCard) -> Bool) -> Program ()
limbo f = do
  stack <- getStack
  let limboed = indexedFilter f stack
  modLimbo $ (++) limboed
  modStack $ indexedFilter (not . f)


unlimbo :: Program ()
unlimbo = do
  limbos <- getLimbo
  modStack ((++) limbos)
  setLimbo []
