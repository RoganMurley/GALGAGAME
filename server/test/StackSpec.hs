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
    [ chainMaskTestCase
    ]


chainMaskTestCase :: TestTree
chainMaskTestCase =
  let
    stackCard :: StackCard
    stackCard = StackCard{
        stackcard_owner = PlayerA
      , stackcard_card = Cards.blazeSword
      }
    stack :: Stack
    stack = Stack.stackFromList [stackCard, stackCard, stackCard, stackCard]
    actual :: Wheel Bool
    actual = Stack.chainMask stack
    expected :: Wheel Bool
    expected = Wheel.init (\i -> i < 4)
  in
    testCase "Chain mask" $ do
      actual @?= expected
