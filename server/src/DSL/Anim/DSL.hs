module DSL.Anim.DSL where

import Bounce (CardBounce)
import Control.Monad.Free (Free(..))
import Card (Card)
import CardAnim (CardAnim, Hurt, Transmute)
import Discard (CardDiscard)
import Life (Life)
import Limbo (CardLimbo)
import Player (WhichPlayer)
import StackCard (StackCard)


data DSL a
  = Null a
  | Raw CardAnim a
  | Hurt WhichPlayer Life Hurt a
  | Heal WhichPlayer Life a
  | Draw WhichPlayer a
  | Reflect a
  | Confound a
  | Reverse a
  | Play WhichPlayer Card Int a
  | Transmute StackCard StackCard Transmute a
  | Mill WhichPlayer Card a
  | GameEnd (Maybe WhichPlayer) a
  | Rotate a
  | Windup a
  | Fabricate StackCard a
  | Bounce [CardBounce] a
  | DiscardStack [CardDiscard] a
  | DiscardHand WhichPlayer [CardDiscard] a
  | Pass WhichPlayer a
  | Limbo [CardLimbo] a
  | Unlimbo a
  deriving (Functor)

type Program a = Free DSL a
