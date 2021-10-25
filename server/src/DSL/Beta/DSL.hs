module DSL.Beta.DSL where

import Card (Card)
import CardAnim (CardAnim, Hurt)
import Control.Monad.Free (Free(..))
import Life (Life)
import Model (Deck, Hand)
import Player (WhichPlayer(..))
import Stack (Stack)
import StackCard (StackCard)
import Transmutation (Transmutation)
import Util (Gen)

import qualified DSL.Alpha.DSL as Alpha


data DSL n
  = Raw (Alpha.Program ()) n
  | Hurt Life WhichPlayer Hurt n
  | Heal Life WhichPlayer n
  | Draw WhichPlayer WhichPlayer Float n
  | AddToHand WhichPlayer Card n
  | Play WhichPlayer Card Int n
  | Transmute (Int -> StackCard -> Maybe Transmutation) n
  | TransmuteHead (StackCard -> StackCard) n
  | TransmuteActive (StackCard -> Maybe StackCard) n
  | Rotate n
  | Windup n
  | Bounce (Int -> StackCard -> Bool) n
  | DiscardStack (Int -> StackCard -> Bool) n
  | DiscardHand WhichPlayer (Int -> Card -> Bool) n
  | MoveStack (Int -> StackCard -> Maybe Int) Int n
  | Mill WhichPlayer Float n
  | GetDeck WhichPlayer (Deck -> n)
  | GetHand WhichPlayer (Hand -> n)
  | GetLife WhichPlayer (Life -> n)
  | GetGen (Gen -> n)
  | GetStack (Stack -> n)
  | GetRot (Int -> n)
  | GetHold (Bool -> n)
  | RawAnim CardAnim n
  | Null n
  deriving (Functor)

type Program = Free DSL
