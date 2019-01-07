module DSL.Anim.DSL where

import Bounce (CardBounce)
import Control.Monad.Free (Free(..))
import Card (Card)
import CardAnim (CardAnim)
import Life (Life)
import Player (WhichPlayer)
import StackCard (StackCard)


data DSL a
  = Null a
  | Raw CardAnim a
  | Slash WhichPlayer Life a
  | Heal WhichPlayer Life a
  | Draw WhichPlayer a
  | Bite WhichPlayer Life a
  | Hubris a
  | Reflect a
  | Reverse a
  | Play WhichPlayer Card Int a
  | Transmute StackCard StackCard a
  | Mill WhichPlayer Card a
  | GameEnd (Maybe WhichPlayer) a
  | Rotate a
  | Windup a
  | Fabricate StackCard a
  | Bounce [CardBounce] a
  deriving (Functor)

type Program a = Free DSL a
