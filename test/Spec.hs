import Test.Tasty
import Test.Tasty.HUnit


import Cards
import Model
import GameState
import Util


-- Helpers.
isEq :: (Eq a, Show a) => a -> a -> Assertion
isEq = assertEqual ""


resolveState :: GameState -> GameState
resolveState (Playing model) = resolveAll model
resolveState s = s

-- fromPlaying
fP :: GameState -> Model
fP (Playing model) = model
fP _               = error "Looks like that state's not a Playing, kid!"


-- Tests.
main :: IO ()
main = defaultMain $
  testGroup "Unit Tests"
    [
      initModelTests
    , cardTests
    , turnEndTests
    ]


initModelTests :: TestTree
initModelTests =
  testGroup "Initial Model"
    [
      testCase "PlayerA has 50 life" $
        isEq (getLife PlayerA model) 50

    , testCase "PlayerB has 50 life" $
        isEq (getLife PlayerB model) 50
    ]
  where
    model = initModel PlayerA (mkGen 0)


cardTests :: TestTree
cardTests =
  testGroup "Cards"
    [
      cardDaggerTests
    , cardHubrisTests
    , cardFireballTests
    ]


cardDummy :: Card
cardDummy =
  Card "Dummy" "Does nothing, just for testing" "" (\_ _ m -> m)


cardDaggerTests :: TestTree
cardDaggerTests =
  testGroup "Dagger Card"
    [
      testCase "Should hurt for 8" $
        case resolveState state of
          Playing model -> do
            isEq (getLife PlayerA model) maxLife
            isEq (getLife PlayerB model) (maxLife - 8)
          _ ->
            assertFailure "Incorrect state"
    ]
  where
    state =
      Playing $
        (initModel PlayerA (mkGen 0))
          { stack = [StackCard PlayerA cardDagger] }


cardHubrisTests :: TestTree
cardHubrisTests =
  testGroup "Hubris Card"
    [
      testCase "Should negate everything to the right" $
        case resolveState state of
          Playing model -> do
            isEq (getLife PlayerA model) maxLife
            isEq (getLife PlayerB model) maxLife
          _ ->
            assertFailure "Incorrect state"
    ]
  where
    state =
      Playing $
        (initModel PlayerA (mkGen 0))
          { stack = [
            StackCard PlayerB cardHubris
          , StackCard PlayerA cardFireball
          , StackCard PlayerB cardFireball
          , StackCard PlayerB cardDummy
          , StackCard PlayerB cardDummy
          ] }


cardFireballTests :: TestTree
cardFireballTests =
  testGroup "Fireball Card"
    [
      testCase "Should hurt for 4 for everything to the right" $
        case resolveState state of
          Playing model -> do
            isEq (getLife PlayerA model) maxLife
            isEq (getLife PlayerB model) (maxLife - 16)
          _ ->
            assertFailure "Incorrect state"
    ]
  where
    state =
      Playing $
        (initModel PlayerA (mkGen 0))
          { stack = [
            StackCard PlayerA cardFireball
          , StackCard PlayerA cardDummy
          , StackCard PlayerB cardDummy
          , StackCard PlayerB cardDummy
          , StackCard PlayerB cardDummy
          ] }


turnEndTests :: TestTree
turnEndTests =
  testGroup "Turn end tests"
    [
      testCase "Ending the turn when it's not your turn does nothing" $
        isEq
          (update EndTurn PlayerB state)
          state
    , testCase "Ending the turn swaps turns" $
        isEq
          (fP (update EndTurn PlayerA state))
          ((fP state) { turn = PlayerB })
    , testCase "Ending the turn twice draws a card for PlayerA" $
        isEq
          (length . (getHand PlayerA) . fP $ endTwice)
          ((length . (getHand PlayerA) $ fP state) + 1)
    , testCase "Ending the turn twice draws a card for PlayerB" $
        isEq
          (length . (getHand PlayerB) . fP $ endTwice)
          ((length . (getHand PlayerB) $ fP state) + 1)
    ]
  where
    state = Playing (initModel PlayerA (mkGen 0))
    endTwice = (update EndTurn PlayerA) . (update EndTurn PlayerB) $ state
