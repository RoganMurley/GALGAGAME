module StackSpec where

import Stack (Stack)
import StackCard (StackCard(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Wheel (Wheel(..))
import Player (WhichPlayer(..))

import qualified Cards
import qualified Stack
import qualified Wheel

tests :: TestTree
tests =
  testGroup "Stack"
    []


stack :: Stack
stack = Stack.stackFromList [stackCard, stackCard, stackCard, stackCard]
  where
    stackCard :: StackCard
    stackCard = StackCard{
        stackcard_owner = PlayerA
      , stackcard_card = Cards.blazeSword
      }
