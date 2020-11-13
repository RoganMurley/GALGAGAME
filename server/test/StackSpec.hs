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
    , chainLengthTestCase
    ]


stack :: Stack
stack = Stack.stackFromList [stackCard, stackCard, stackCard, stackCard]
  where
    stackCard :: StackCard
    stackCard = StackCard{
        stackcard_owner = PlayerA
      , stackcard_card = Cards.blazeSword
      }


chainMaskTestCase :: TestTree
chainMaskTestCase =
  testCase "Chain mask" $
    Stack.chainMask stack @?= Wheel.init (\i -> i < 4)


chainFilterTestCase :: TestTree
chainFilterTestCase =
  let
    expected :: Stack
    expected = Stack.init { wheel_0 = wheel_0 stack, wheel_2 = wheel_2 stack }
  in
  testCase "Chain filter" $
    Stack.chainFilter (\i _ -> i `mod` 2 == 1) stack @?= expected


chainLengthTestCase :: TestTree
chainLengthTestCase =
  testCase "Chain length" $
    Stack.chainLength stack @?= 4
