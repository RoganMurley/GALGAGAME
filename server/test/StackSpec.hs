module StackSpec where

import qualified Cards
import GameState (initModel)
import Model (Model(..))
import Player (WhichPlayer (..))
import qualified Stack
import StackCard (StackCard (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Util (mkGen)

import qualified DSL.Alpha as Alpha

tests :: TestTree
tests =
  testGroup
    "Stack"
    [testCase "moveStack" testMoveStack]

testMoveStack :: Assertion
testMoveStack = actual @?= expected
  where
    stack = Stack.set Stack.init 1 (Just $ StackCard { stackcard_owner = PlayerA, stackcard_card = Cards.blazeSword })
    model = (initModel PlayerA Nothing Nothing (mkGen 0)) {model_stack = stack}
    prog = Alpha.moveStack (\i _ -> Just (i + 1))
    actual = model_stack $ Alpha.modI model prog
    expected = Stack.windup stack