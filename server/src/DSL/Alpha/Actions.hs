{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module DSL.Alpha.Actions where

import Card (Card)
import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import Data.List (partition)
import DSL.Alpha.DSL (DSL(..), Program)
import Player (WhichPlayer(..), other)
import Life (Life)
import Model (Deck, Hand, Passes(..), Stack, Turn, maxHandLength)
import Safe (headMay, tailSafe)
import StackCard(StackCard(StackCard), isOwner)

import {-# SOURCE #-} Cards (theEnd)


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


modStackAll :: (StackCard -> StackCard) -> Program ()
modStackAll f = modStack $ fmap f


modTurn :: (Turn -> Turn) -> Program ()
modTurn f = getTurn >>= (setTurn . f)


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


play :: WhichPlayer -> Card -> Program ()
play w c = modStack $ (:) (StackCard w c)


bounceAll :: WhichPlayer -> Program ()
bounceAll w = do
  (ours, theirs) <- partition (isOwner w) <$> getStack
  setStack theirs
  let oursCards = (\(StackCard _ c) -> c) <$> ours
  modHand w $ \h -> h ++ oursCards


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


draw :: WhichPlayer -> Program ()
draw w =
  do
    deck <- getDeck w
    case headMay deck of
      Just card -> do
        modDeck w tailSafe
        addToHand w card
      Nothing ->
        addToHand w theEnd


transmute :: Card -> Program ()
transmute c = do
  modStackHead (\(StackCard o _) -> StackCard o c)


setHeadOwner :: WhichPlayer -> Program ()
setHeadOwner w = do
  modStackHead (\(StackCard _ c) -> StackCard w c)
