module DSL.Beta.DSL where

import Card (Card)
import Control.Monad.Free (Free(..))
import Player (WhichPlayer(..))
import Util (Gen)
import Life (Life)
import Model (Deck, Hand, Stack)

import qualified DSL.Alpha.DSL as Alpha


data DSL n
  = Raw (Alpha.Program ()) n
  | Slash Life WhichPlayer n
  | Heal Life WhichPlayer n
  | Draw WhichPlayer n
  | Bite Life WhichPlayer n
  | AddToHand WhichPlayer Card n
  | Obliterate n
  | Reverse n
  | Play WhichPlayer Card n
  | Transmute Card n
  | SetHeadOwner WhichPlayer n
  | GetDeck WhichPlayer (Deck -> n)
  | GetHand WhichPlayer (Hand -> n)
  | GetLife WhichPlayer (Life -> n)
  | GetGen (Gen -> n)
  | GetStack (Stack -> n)
  | Null n
  deriving (Functor)

type Program = Free DSL
