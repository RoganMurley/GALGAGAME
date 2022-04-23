module DSL.Anim.DSL where

import Bounce (CardBounce)
import Card (Card)
import CardAnim (CardAnim, Hurt, TimeModifier)
import Control.Monad.Free (Free (..))
import Discard (CardDiscard)
import HandCard (HandCard)
import Life (Life)
import Player (WhichPlayer)
import Transmutation (Transmutation)
import Wheel (Wheel)

data DSL a
  = Null a
  | Raw CardAnim a
  | Hurt WhichPlayer Life Hurt a
  | Heal WhichPlayer Life a
  | Draw WhichPlayer TimeModifier a
  | Play WhichPlayer HandCard Int a
  | Transmute (Wheel (Maybe Transmutation)) a
  | Mill WhichPlayer Card TimeModifier a
  | GameEnd (Maybe WhichPlayer) a
  | Rotate a
  | Windup a
  | Bounce (Wheel (Maybe CardBounce)) TimeModifier a
  | DiscardStack (Wheel Bool) a
  | DiscardHand WhichPlayer [CardDiscard] a
  | Reveal WhichPlayer [Bool] a
  | MoveStack (Wheel (Maybe Int)) TimeModifier a
  | Pass WhichPlayer a
  | GetGen a
  deriving (Functor)

type Program a = Free DSL a
