module DSL.Beta.DSL where

import Card (Card)
import CardAnim (CardAnim, Hurt)
import Control.Monad.Free (Free(..))
import Life (Life)
import Model (Deck, Hand, Limbo, Stack)
import Player (WhichPlayer(..))
import StackCard (StackCard)
import Transmutation (Transmutation)
import Util (Gen)

import qualified DSL.Alpha.DSL as Alpha


data DSL n
  = Raw (Alpha.Program ()) n
  | Hurt Life WhichPlayer Hurt n
  | Heal Life WhichPlayer n
  | Draw WhichPlayer WhichPlayer n
  | AddToHand WhichPlayer Card n
  | Confound n
  | Reverse n
  | Play WhichPlayer Card Int n
  | Transmute ((Int, StackCard) -> Transmutation) n
  | Rotate n
  | Windup n
  | Fabricate StackCard n
  | Bounce (StackCard -> Bool) n
  | DiscardStack ((Int, StackCard) -> Bool) n
  | DiscardHand WhichPlayer ((Int, Card) -> Bool) n
  | Limbo ((Int, StackCard) -> Bool) n
  | Unlimbo n
  | GetDeck WhichPlayer (Deck -> n)
  | GetHand WhichPlayer (Hand -> n)
  | GetLife WhichPlayer (Life -> n)
  | GetGen (Gen -> n)
  | GetStack (Stack -> n)
  | GetLimbo (Limbo -> n)
  | GetRot (Int -> n)
  | RawAnim CardAnim n
  | Null n
  deriving (Functor)

type Program = Free DSL
