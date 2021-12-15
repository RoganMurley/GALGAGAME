module DSL.Beta.DSL where

import Card (Card)
import CardAnim (CardAnim, Hurt)
import Control.Monad.Free (Free(..))
import HandCard (HandCard)
import Life (Life)
import Model (Deck, Hand, Model)
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
  | AddToHand WhichPlayer HandCard n
  | Play WhichPlayer HandCard Int n
  | Transmute (Int -> StackCard -> Maybe Transmutation) n
  | TransmuteActive (StackCard -> Maybe StackCard) n
  | Rotate n
  | Windup n
  | Bounce (Int -> StackCard -> Bool) n
  | DiscardStack (Int -> StackCard -> Bool) n
  | DiscardHand WhichPlayer (Int -> Card -> Bool) n
  | MoveStack (Int -> StackCard -> Maybe Int) Int n
  | Mill WhichPlayer Float n
  | Reveal WhichPlayer (Int -> Card -> Bool) n
  | GetDeck WhichPlayer (Deck -> n)
  | GetHand WhichPlayer (Hand -> n)
  | GetLife WhichPlayer (Life -> n)
  | GetGen (Gen -> n)
  | GetStack (Stack -> n)
  | GetRot (Int -> n)
  | GetHold (Bool -> n)
  | GetModel (Model -> n)
  | RawAnim CardAnim n
  | Null n
  deriving (Functor)

type Program = Free DSL
