module DSL.Beta.DSL where

import Card (Card)
import CardAnim (CardAnim)
import Control.Monad.Free (Free(..))
import Life (Life)
import Model (Deck, Hand, Stack)
import Player (WhichPlayer(..))
import StackCard (StackCard)
import Util (Gen)

import qualified DSL.Alpha.DSL as Alpha


data DSL n
  = Raw (Alpha.Program ()) n
  | Slash Life WhichPlayer n
  | Heal Life WhichPlayer n
  | Draw WhichPlayer n
  | Bite Life WhichPlayer n
  | Curse Life WhichPlayer n
  | AddToHand WhichPlayer Card n
  | Hubris n
  | Reflect n
  | Confound n
  | Reverse n
  | Play WhichPlayer Card Int n
  | Transmute Card n
  | Rotate n
  | Fabricate StackCard n
  | Bounce (StackCard -> Bool) n
  | SetHeadOwner WhichPlayer n
  | GetDeck WhichPlayer (Deck -> n)
  | GetHand WhichPlayer (Hand -> n)
  | GetLife WhichPlayer (Life -> n)
  | GetGen (Gen -> n)
  | GetStack (Stack -> n)
  | RawAnim CardAnim n
  | Null n
  deriving (Functor)

type Program = Free DSL
