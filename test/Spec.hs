import Test.Tasty
import Test.Tasty.HUnit


import Cards
import Model
import GameState
import Util


-- Helpers.
isEq :: (Eq a, Show a) => a -> a -> Assertion
isEq = assertEqual ""

isTrue :: Bool -> Assertion
isTrue = assertBool ""

isFalse :: Bool -> Assertion
isFalse = (assertBool "") . not


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
    , cardBoomerangTests
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
            isEq maxLife       (getLife PlayerA model)
            isEq (maxLife - 8) (getLife PlayerB model)
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
            isEq maxLife (getLife PlayerA model)
            isEq maxLife (getLife PlayerB model)
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
            isEq maxLife        (getLife PlayerA model)
            isEq (maxLife - 16) (getLife PlayerB model)
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

cardBoomerangTests :: TestTree
cardBoomerangTests =
  testGroup "Boomerang Card"
    [
      testCase "Should hurt for 2" $
        case resolveState state of
          Playing model -> do
            isEq maxLife        (getLife PlayerA model)
            isEq (maxLife - 2)  (getLife PlayerB model)
          _ ->
            assertFailure "Incorrect state"
    , testCase "Comes back to hand" $
        case resolveState state of
          Playing model ->
            let hand = getHand PlayerA model in
              isTrue (elem cardBoomerang hand)
          _ ->
            assertFailure "Incorrect state"
    , testCase "Doesn't come back to hand if hand is full" $
        case resolveState (fullHandState) of
          Playing model ->
            let hand = getHand PlayerA model in
              isFalse (elem cardBoomerang hand)
          _ ->
            assertFailure "Incorrect state"
    ]
  where
    state =
      Playing $
        (initModel PlayerA (mkGen 0))
          { stack = [
            StackCard PlayerA cardBoomerang
          ] }
    fullHandState =
      Playing . (setHand PlayerA (replicate 6 cardDummy)) $ fP state


turnEndTests :: TestTree
turnEndTests =
  testGroup "Turn end tests"
    [
      testCase "Ending the turn when it's not your turn does nothing" $
        isEq
          state
          (update EndTurn PlayerB state)
    , testCase "Ending the turn when your hand is full does nothing" $
        isEq
          fullHandState
          (update EndTurn PlayerA fullHandState)
    , testCase "Ending the turn swaps the turn player" $
        isEq
          PlayerB
          (turn . fP $ update EndTurn PlayerA state)
    , testCase "Ending the turn increments the passes count" $
        isEq
          OnePass
          (passes . fP $ update EndTurn PlayerA state)
    , testCase "Ending the turn twice resets the passes count" $
        isEq
          NoPass
          (passes . fP $ endTwice)
    , testCase "Ending the turn twice draws a card for PlayerA" $
        isEq
          ((length . (getHand PlayerA) $ fP state) + 1)
          (length . (getHand PlayerA) . fP $ endTwice)
    , testCase "Ending the turn twice draws a card for PlayerB" $
        isEq
          ((length . (getHand PlayerB) $ fP state) + 1)
          (length . (getHand PlayerB) . fP $ endTwice)
    ]
  where
    state = Playing (initModel PlayerA (mkGen 0))
    fullHandState = Playing . (drawCard PlayerA) . (drawCard PlayerA) $ initModel PlayerA (mkGen 0)
    endTwice = (update EndTurn PlayerB) . (update EndTurn PlayerA) $ state
