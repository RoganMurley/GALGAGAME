module GameCommand where

import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Safe (headMay, tailSafe)

import Characters (CharModel(..), SelectedCharacters(..), selectChar, initCharModel)
import GameState (GameState(..), PlayState(..), getStateGen, initModel)
import Model
import Player (WhichPlayer(..), other)
import Username (Username)
import Util (Err, Gen, split)

import qualified Outcome as Outcome
import Outcome (Outcome)


data GameCommand =
    EndTurn
  | PlayCard Int
  | HoverCard (Maybe Int)
  | Rematch
  | Concede
  | SelectCharacter Text
  | Chat Username Text
  deriving (Show)


update :: GameCommand -> WhichPlayer -> GameState -> Either Err (Maybe GameState, [Outcome])
update (Chat username msg) _ _ = Right (Nothing, [Outcome.Encodable $ Outcome.Chat username msg])
update Concede which state =
  Right (Just . Started $ Ended (Just (other which)) (getStateGen state), [Outcome.Sync])
update cmd which state =
  case state of
    Waiting _ ->
      Left ("Unknown command " <> (cs $ show cmd) <> " on a waiting GameState")
    Selecting selectModel turn gen ->
      case cmd of
        SelectCharacter n ->
          let
            startIfBothReady :: GameState -> GameState
            startIfBothReady (Selecting (CharModel (ThreeSelected c1 c2 c3) (ThreeSelected ca cb cc) _) _ _) =
              Started . Playing $ initModel turn (c1, c2, c3) (ca, cb, cc) gen
            startIfBothReady s = s
          in
            Right (Just . startIfBothReady $ Selecting (selectChar selectModel which n) turn gen, [Outcome.Sync])
        _ ->
          Left ("Unknown command " <> (cs $ show cmd) <> " on a selecting GameState")
    Started started ->
      case started of
        Playing model ->
          case cmd of
            EndTurn ->
              endTurn which model
            PlayCard index ->
              ((\x -> (x, [Outcome.Sync, Outcome.PlayCard which])) . Just . Started . Playing) <$> (playCard index which model)
            HoverCard index ->
              (\x -> (Nothing, [x])) <$> (hoverCard index which model)
            _ ->
              Left ("Unknown command " <> (cs $ show cmd) <> " on a Playing GameState")
        Ended winner gen ->
          case cmd of
            Rematch ->
              Right . (\x -> (x, [Outcome.Sync])) . Just $ Selecting initCharModel (fromMaybe PlayerA winner) (fst $ split gen)
            _ ->
              Left ("Unknown command " <> (cs $ show cmd) <> " on an Ended GameState")


endTurn :: WhichPlayer -> Model -> Either Err (Maybe GameState, [Outcome])
endTurn which model
  | turn /= which = Left "You can't end the turn when it's not your turn"
  | handFull      = Left "You can't end the turn when your hand is full"
  | otherwise     =
    case passes of
      OnePass ->
        case runWriter . resolveAll $ model of
          (Playing m, res) ->
            let newState = Started . Playing . drawCards . resetPasses . swapTurn $ m in
              Right (Just newState, [Outcome.Encodable $ Outcome.Resolve res newState, Outcome.EndTurn which])
          (Ended w g, res) ->
            let newState = Started (Ended w g) in
              Right (Just newState, [Outcome.Encodable $ Outcome.Resolve res newState, Outcome.EndTurn which])
      NoPass ->
        Right (Just . Started . Playing . swapTurn $ model, [Outcome.Sync])
  where
    turn = getTurn model :: Turn
    passes = getPasses model :: Passes
    handFull = length (getHand which model) >= maxHandLength :: Bool
    drawCards :: Model -> Model
    drawCards m = (drawCard PlayerA) . (drawCard PlayerB) $ m


resolveAll :: Model -> Writer [Model] PlayState
resolveAll model
  | null stack = return (Playing model)
  | otherwise =
    do
      tell [model]
      case resolveOne model of
        Playing newModel ->
          resolveAll newModel
        Ended which gen ->
          return (Ended which gen)
  where
    stack = getStack model :: Stack
    resolveOne :: Model -> PlayState
    resolveOne m =
      lifeGate . eff $ modStack tailSafe m
      where
        eff :: Model -> Model
        eff =
          case headMay stack of
            Nothing ->
              id
            Just (StackCard o c) ->
              (card_eff c) o

lifeGate :: Model -> PlayState
lifeGate m
  | lifePA <= 0 && lifePB <= 0 =
    Ended Nothing gen
  | lifePB <= 0 =
    Ended (Just PlayerA) gen
  | lifePA <= 0 =
    Ended (Just PlayerB) gen
  | otherwise =
    Playing
      . (setLife PlayerA (min maxLife lifePA))
      . (setLife PlayerB (min maxLife lifePB))
      $ m
  where
    gen = getGen m :: Gen
    lifePA = getLife PlayerA m :: Life
    lifePB = getLife PlayerB m :: Life


hoverCard :: Maybe Int -> WhichPlayer -> Model -> Either Err Outcome
hoverCard index which model =
  case index of
    Just i ->
      if i < (length . (getHand which) $ model)
        then
          Right . Outcome.Encodable $ Outcome.Hover which (Just i)
        else
          Left ("Hover index out of bounds (" <> (cs . show $ i ) <> ")" :: Err)
    Nothing ->
      Right . Outcome.Encodable $ Outcome.Hover which Nothing
