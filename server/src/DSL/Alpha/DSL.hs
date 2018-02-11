module DSL.Alpha.DSL where

import Control.Monad.Free (Free)
import Player (WhichPlayer)
import Util (Gen)
import Model (Deck, Hand, Life, Passes, Stack, Turn)


data DSL n =
    GetGen (Gen -> n)
  | GetDeck WhichPlayer (Deck -> n)
  | GetHand WhichPlayer (Hand-> n)
  | GetLife WhichPlayer (Life -> n)
  | GetPasses (Passes -> n)
  | GetStack (Stack -> n)
  | GetTurn (Turn -> n)
  | SetGen Gen n
  | SetDeck WhichPlayer Deck n
  | SetHand WhichPlayer Hand n
  | SetLife WhichPlayer Life n
  | SetPasses Passes n
  | SetStack Stack n
  | SetTurn Turn n
  deriving (Functor)


type Program = Free DSL
