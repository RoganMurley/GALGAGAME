{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit

import System.Random (mkStdGen, StdGen)

import GameState

isEq = assertEqual ""


main :: IO ()
main = defaultMain $
  testGroup "Unit Tests"
    [
      initModelTests
    , cardTests
    ]


initModelTests =
  testGroup "Initial Model"
    [
      testCase "PlayerA has 50 life" $
        isEq (getLife PlayerA model) 50

    , testCase "PlayerB has 50 life" $
        isEq (getLife PlayerB model) 50
    ]
  where
    model = initModel PlayerA (mkStdGen 0)


cardTests =
  testGroup "Cards"
    [
      cardDaggerTests
    , cardHubrisTests
    , cardFireballTests
    ]


cardDummy :: Card
cardDummy =
  Card "Dummy" "Does nothing, just for testing" "" (\_ x -> x)

cardDaggerTests =
  testGroup "Dagger Card"
    [
      testCase "Should hurt for 8" $
        case resolveAll state of
          Playing model -> do
            isEq (getLife PlayerA model) lifeMax
            isEq (getLife PlayerB model) (lifeMax - 8)
          otherwise ->
            assertFailure "Incorrect state"
    ]
  where
    state =
      Playing $
        setStack [StackCard PlayerA cardDagger] $
          initModel PlayerA (mkStdGen 0)

cardHubrisTests =
  testGroup "Hubris Card"
    [
      testCase "Should negate everything to the right" $
        case resolveAll state of
          Playing model -> do
            isEq (getLife PlayerA model) lifeMax
            isEq (getLife PlayerB model) lifeMax
          otherwise ->
            assertFailure "Incorrect state"
    ]
  where
    state =
      Playing $
        setStack
          [
            StackCard PlayerB cardHubris
          , StackCard PlayerA cardFireball
          , StackCard PlayerB cardFireball
          , StackCard PlayerB cardDummy
          , StackCard PlayerB cardDummy
          ] $
            initModel PlayerA (mkStdGen 0)


cardFireballTests =
  testGroup "Fireball Card"
    [
      testCase "Should hurt for four for everything to the right" $
        case resolveAll state of
          Playing model -> do
            isEq (getLife PlayerA model) lifeMax
            isEq (getLife PlayerB model) (lifeMax - 16)
          otherwise ->
            assertFailure "Incorrect state"
    ]
  where
    state =
      Playing $
        setStack
          [
            StackCard PlayerA cardFireball
          , StackCard PlayerB cardDummy
          , StackCard PlayerA cardDummy
          , StackCard PlayerB cardDummy
          , StackCard PlayerB cardDummy
          ] $
            initModel PlayerA (mkStdGen 0)
