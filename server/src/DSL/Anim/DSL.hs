{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module DSL.Anim.DSL where

import Bounce (CardBounce)
import Card (Card)
import CardAnim (CardAnim, Hurt, TimeModifier)
import Control.Monad.Freer (Eff)
import Data.Text (Text)
import Discard (CardDiscard)
import HandCard (HandCard)
import Life (Life)
import Player (WhichPlayer)
import Transmutation (Transmutation)
import Wheel (Wheel)

data DSL n where
  Null :: DSL ()
  Raw :: CardAnim -> DSL ()
  Hurt :: WhichPlayer -> Life -> Hurt -> DSL ()
  Heal :: WhichPlayer -> Life -> DSL ()
  Draw :: WhichPlayer -> TimeModifier -> DSL ()
  Play :: WhichPlayer -> HandCard -> Int -> DSL ()
  Transmute :: Wheel (Maybe Transmutation) -> DSL ()
  Mill :: WhichPlayer -> Card -> TimeModifier -> DSL ()
  GameEnd :: Maybe WhichPlayer -> DSL ()
  Rotate :: DSL ()
  Windup :: DSL ()
  Bounce :: Wheel (Maybe CardBounce) -> TimeModifier -> DSL ()
  BounceDeck :: Wheel Bool -> TimeModifier -> DSL ()
  DiscardStack :: Wheel Bool -> DSL ()
  DiscardHand :: WhichPlayer -> [CardDiscard] -> DSL ()
  Reveal :: WhichPlayer -> [Bool] -> DSL ()
  MoveStack :: Wheel (Maybe Int) -> TimeModifier -> DSL ()
  Pass :: WhichPlayer -> DSL ()
  GetGen :: DSL ()
  UnknownDamage :: DSL ()
  Announce :: Text -> TimeModifier -> DSL ()

type Program = Eff '[DSL]
