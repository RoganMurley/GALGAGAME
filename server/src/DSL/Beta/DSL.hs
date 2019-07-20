module DSL.Beta.DSL where

import Card (Card)
import CardAnim (CardAnim, Hurt, Transmute)
import Control.Monad.Free (Free(..))
import Life (Life)
import Model (Deck, Hand, Stack)
import Player (WhichPlayer(..))
import StackCard (StackCard)
import Util (Gen)

import qualified DSL.Alpha.DSL as Alpha


data DSL n
  = Raw (Alpha.Program ()) n
  | Hurt Life WhichPlayer Hurt n
  | Heal Life WhichPlayer n
  | Draw WhichPlayer WhichPlayer n
  | AddToHand WhichPlayer Card n
  | Reflect n
  | Confound n
  | Reverse n
  | Play WhichPlayer Card Int n
  | Transmute Card Transmute n
  | Rotate n
  | Windup n
  | Fabricate StackCard n
  | Bounce (StackCard -> Bool) n
  | Discard ((Int, StackCard) -> Bool) n
  | SetHeadOwner WhichPlayer n
  | GetDeck WhichPlayer (Deck -> n)
  | GetHand WhichPlayer (Hand -> n)
  | GetLife WhichPlayer (Life -> n)
  | GetGen (Gen -> n)
  | GetStack (Stack -> n)
  | GetRot (Int -> n)
  | RawAnim CardAnim n
  | Null n
  deriving (Functor)

type Program = Free DSL
