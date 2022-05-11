module DSL.Beta.DSL where

import Bounce (CardBounce)
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
import Wheel (Wheel)

data DSL n
  = Raw (Alpha.Program ()) n
  | Hurt Life WhichPlayer Hurt n
  | Heal Life WhichPlayer n
  | Draw WhichPlayer WhichPlayer TimeModifier n
  | AddToHand WhichPlayer HandCard n
  | Play WhichPlayer HandCard Int n
  | Transmute' (Wheel (Maybe Transmutation)) n -- (Int -> StackCard -> Maybe Transmutation)
  | TransmuteActive (StackCard -> Maybe StackCard) n
  | Rotate n
  | Windup n
  | Bounce' (Wheel (Maybe CardBounce)) TimeModifier n -- (Int -> StackCard -> Bool)
  | DiscardStack' (Wheel Bool) n -- Int -> StackCard -> Bool
  | DiscardHand WhichPlayer (Int -> Card -> Bool) n
  | MoveStack' (Wheel (Maybe Int)) TimeModifier n -- (Int -> StackCard -> Maybe Int)
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
