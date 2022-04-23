module DSL.Beta.DSL where

import Card (Card)
import CardAnim (CardAnim, Hurt, TimeModifier)
import Control.Monad.Free (Free (..))
import qualified DSL.Alpha.DSL as Alpha
import HandCard (HandCard)
import Life (Life)
import Model (Deck, Hand, Model)
import Player (WhichPlayer (..))
import Stack (Stack)
import StackCard (StackCard)
import Transmutation (Transmutation)
import Util (Gen)

data DSL n
  = Raw (Alpha.Program ()) n
  | Hurt Life WhichPlayer Hurt n
  | Heal Life WhichPlayer n
  | Draw WhichPlayer WhichPlayer TimeModifier n
  | AddToHand WhichPlayer HandCard n
  | Play WhichPlayer HandCard Int n
  | Transmute (Int -> StackCard -> Maybe Transmutation) n
  | TransmuteActive (StackCard -> Maybe StackCard) n
  | Rotate n
  | Windup n
  | Bounce (Int -> StackCard -> Bool) TimeModifier n
  | DiscardStack (Int -> StackCard -> Bool) n
  | DiscardHand WhichPlayer (Int -> Card -> Bool) n
  | MoveStack (Int -> StackCard -> Maybe Int) TimeModifier n
  | Mill WhichPlayer TimeModifier n
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
