module DSL.Anim.DSL where

import Control.Monad.Free (Free(..))
import Card (Card)
import Life (Life)
import Player (WhichPlayer)
import StackCard (StackCard)


data DSL a
  = Null a
  | Slash WhichPlayer Life a
  | Heal WhichPlayer a
  | Draw WhichPlayer a
  | Obliterate a
  | Reverse a
  | Play WhichPlayer Card a
  | Transmute StackCard StackCard a
  deriving (Functor)


type Program a = Free DSL a
