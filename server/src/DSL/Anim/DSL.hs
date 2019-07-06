module DSL.Anim.DSL where

import Bounce (CardBounce)
import Control.Monad.Free (Free(..))
import Card (Card)
import CardAnim (CardAnim, Hurt, Transmute)
import Life (Life)
import Player (WhichPlayer)
import StackCard (StackCard)


data DSL a
  = Null a
  | Raw CardAnim a
  | Hurt WhichPlayer Life Hurt a
  | Heal WhichPlayer Life a
  | Draw WhichPlayer a
  | Hubris a
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
  | Pass WhichPlayer a
  | Infinity a
  deriving (Functor)

type Program a = Free DSL a
