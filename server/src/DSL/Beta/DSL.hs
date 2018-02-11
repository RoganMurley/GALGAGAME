module DSL.Beta.DSL where

import Control.Monad.Free (Free(..))
import Player (WhichPlayer(..))
import Util (Gen)
import Model (Card, Deck, Hand, Life, Stack)

import qualified DSL.Alpha.DSL as Alpha


data DSL n
  = Raw (Alpha.Program ()) n
  | Slash Life WhichPlayer n
  | Heal Life WhichPlayer n
  | Draw WhichPlayer n
  | AddToHand WhichPlayer Card n
  | Obliterate n
  | Reverse n
  | Play WhichPlayer Card n
  | GetDeck WhichPlayer (Deck -> n)
  | GetHand WhichPlayer (Hand -> n)
  | GetLife WhichPlayer (Life -> n)
  | GetGen (Gen -> n)
  | GetStack (Stack -> n)
  | Null n
  deriving (Functor)

type Program = Free DSL
