module GameSpec where

import Card (Card (..), Status (..), addStatus)
import Cards qualified
import DSL.Alpha qualified as Alpha
import GameState (initModel)
import Model (Model (..), PlayerModel (..))
import Player (WhichPlayer (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Util (mkGen)

tests :: TestTree
tests =
  testGroup
    "Game"
    [testDamage, testAddStatusNonLethal, testAddStatusEcho, testStatusOrder]

testProgram :: (Eq a, Show a) => String -> a -> (Model -> a) -> Alpha.Program () -> TestTree
testProgram testName expected f prog = testCase testName $ do
  let initial = initModel PlayerA Nothing Nothing (mkGen 0)
  let model = Alpha.modI initial prog
  f model @?= expected

testDamage :: TestTree
testDamage =
  testProgram
    "testDamage"
    40
    (pmodel_life . model_pa)
    $ Alpha.hurt 10 PlayerA

testAddStatusNonLethal :: TestTree
testAddStatusNonLethal = testCase "testAddStatusNonLethal" $ do
  let card = addStatus StatusNonLethal Cards.mercySword
  card_statuses card @?= [StatusNonLethal]

testAddStatusEcho :: TestTree
testAddStatusEcho = testCase "testAddStatusEcho" $ do
  let card = addStatus StatusEcho . addStatus StatusEcho $ Cards.waterSword
  card_statuses card @?= [StatusEcho, StatusEcho]

testStatusOrder :: TestTree
testStatusOrder = testCase "testStatusOrder" $ do
  let cardA = addStatus StatusNonLethal . addStatus (StatusBonusDamage 10) $ Cards.waterSword
  card_statuses cardA @?= [StatusBonusDamage 10, StatusNonLethal]
  let cardB = addStatus (StatusBonusDamage 10) . addStatus StatusNonLethal $ Cards.waterSword
  card_statuses cardB @?= [StatusBonusDamage 10, StatusNonLethal]
