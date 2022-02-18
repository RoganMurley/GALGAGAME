module DSL.Alpha.DSL where

import Control.Monad.Free (Free)
import Life (Life)
import Model (Deck, Hand, Model, Passes, Turn)
import Player (WhichPlayer)
import Stack (Stack)
import Util (Gen)

data DSL n
  = GetGen (Gen -> n)
  | GetDeck WhichPlayer (Deck -> n)
  | GetHand WhichPlayer (Hand -> n)
  | GetLife WhichPlayer (Life -> n)
  | GetMaxLife WhichPlayer (Life -> n)
  | GetPasses (Passes -> n)
  | GetStack (Stack -> n)
  | GetTurn (Turn -> n)
  | GetRot (Int -> n)
  | GetHold (Bool -> n)
  | GetModel (Model -> n)
  | SetGen Gen n
  | SetDeck WhichPlayer Deck n
  | SetHand WhichPlayer Hand n
  | SetLife WhichPlayer Life n
  | SetPasses Passes n
  | SetStack Stack n
  | SetTurn Turn n
  | SetRot Int n
  | SetHold Bool n
  deriving (Functor)

type Program = Free DSL
