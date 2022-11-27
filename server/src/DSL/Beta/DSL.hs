{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module DSL.Beta.DSL where

import Bounce (CardBounce)
import Card (Card)
import CardAnim (CardAnim, Hurt, TimeModifier)
import Control.Monad.Freer (Eff)
import qualified DSL.Alpha.DSL as Alpha
import HandCard (HandCard)
import Life (Life)
import Model (Deck, Hand, Model)
import Player (WhichPlayer (..))
import Stack (Stack)
import Transmutation (Transmutation)
import Util (Gen)
import Wheel (Wheel)

data DSL n where
  Raw :: Alpha.Program () -> DSL ()
  Hurt :: Life -> WhichPlayer -> Hurt -> DSL ()
  Heal :: Life -> WhichPlayer -> DSL ()
  Draw' :: WhichPlayer -> WhichPlayer -> TimeModifier -> DSL ()
  AddToHand :: WhichPlayer -> HandCard -> DSL ()
  Play :: WhichPlayer -> HandCard -> Int -> DSL ()
  Transmute' :: Wheel (Maybe Transmutation) -> DSL ()
  TransmuteActive' :: Transmutation -> DSL ()
  Rotate :: DSL ()
  Windup :: DSL ()
  Bounce' :: Wheel (Maybe CardBounce) -> TimeModifier -> DSL ()
  DiscardStack' :: Wheel Bool -> DSL ()
  DiscardHand :: WhichPlayer -> (Int -> Card -> Bool) -> DSL ()
  MoveStack' :: Wheel (Maybe Int) -> TimeModifier -> DSL ()
  Mill :: WhichPlayer -> TimeModifier -> DSL ()
  Reveal :: WhichPlayer -> (Int -> Card -> Bool) -> DSL ()
  GetDeck :: WhichPlayer -> DSL Deck
  GetHand :: WhichPlayer -> DSL Hand
  GetLife :: WhichPlayer -> DSL Life
  GetGen :: DSL Gen
  GetStack :: DSL Stack
  GetRot :: DSL Int
  GetHold :: DSL Bool
  GetModel :: DSL Model
  RawAnim :: CardAnim -> DSL ()
  UnknownDamage :: DSL ()
  Null :: DSL ()

type Program = Eff '[DSL]
