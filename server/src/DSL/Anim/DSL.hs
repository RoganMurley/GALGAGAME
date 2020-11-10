module DSL.Anim.DSL where

import Bounce (CardBounce)
import Control.Monad.Free (Free(..))
import Card (Card)
import CardAnim (CardAnim, Hurt)
import Discard (CardDiscard)
import Life (Life)
import Player (WhichPlayer)
import StackCard (StackCard)
import Transmutation (Transmutation)
import Wheel (Wheel)


data DSL a
  = Null a
  | Raw CardAnim a
  | Hurt WhichPlayer Life Hurt a
  | Heal WhichPlayer Life a
  | Draw WhichPlayer a
  | Confound a
  | Reverse a
  | Play WhichPlayer Card Int a
  | Transmute (Wheel (Maybe Transmutation)) a
  | Mill WhichPlayer Card a
  | GameEnd (Maybe WhichPlayer) a
  | Rotate a
  | Windup a
  | Fabricate StackCard a
  | Bounce (Wheel (Maybe CardBounce)) a
  | DiscardStack (Wheel Bool) a
  | DiscardHand WhichPlayer [CardDiscard] a
  | Pass WhichPlayer a
  deriving (Functor)

type Program a = Free DSL a
