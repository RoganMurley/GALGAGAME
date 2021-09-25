{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module ArtificalIntelligence where

import Control.Monad.Trans.Writer (runWriter)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.String.Conversions (cs)
import DSL.Alpha

import Card (Card)
import GameCommand (GameCommand(..), resolveAll, update)
import GameState
import Mirror (mirror)
import Model
import Player (WhichPlayer(..))
import Scenario (Scenario(..))
import Stack (diasporaLength)
import Util (Err, Gen)

import qualified Replay.Active
import qualified Cards


type Weight = Int
data Action = PlayAction Int | EndAction
  deriving (Show)


evalResult :: Either Err PlayState -> Weight
evalResult (Right state) = evalState state
evalResult (Left err)    = error (cs err)


evalState :: PlayState -> Weight
evalState (Ended (Just PlayerA) _ _ _) = 100
evalState (Ended (Just PlayerB) _ _ _) = -200
evalState (Ended Nothing _  _ _)       = 50
evalState (Playing model _)            = evalModel model
  where
    evalModel :: Model -> Weight
    evalModel m
      | (diasporaLength $ evalI m $ getStack) > 0 =
        evalState . fst . runWriter $ resolveAll m Replay.Active.null 0
      | otherwise =
          (evalPlayer PlayerA m) - (evalPlayer PlayerB m)
    evalPlayer :: WhichPlayer -> Model -> Weight
    evalPlayer which m =
      evalI m $ do
        life <- getLife which
        hand <- getHand which
        return (life + 7 * (length hand) + (sum . (fmap biasHand) $ hand))


toCommand :: Action -> GameCommand
toCommand (PlayAction i) = PlayCard i
toCommand EndAction      = EndTurn


possibleActions :: Model -> [Action]
possibleActions m = endAction ++ (PlayAction <$> xs)
  where
    handLength :: Int
    handLength = length $ evalI m $ getHand PlayerA
    xs :: [Int]
    xs = [ x | x <- [0..maxHandLength], x < handLength]
    endAction :: [Action]
    endAction
      | handLength == maxHandLength = []
      | otherwise = [ EndAction ]


postulateAction :: WhichPlayer -> Model -> Gen -> Scenario -> Action -> Either Err PlayState
postulateAction which model gen scenario action =
  let
    command = toCommand action :: GameCommand
    state = Started $ Playing (modI model $ setGen gen) (Replay.Active.null) :: GameState
    result = update command which state scenario (Nothing, Nothing)
  in
    case result of
      Left err ->
        Left err
      Right (Just s, _) ->
        case s of
          Started started ->
            Right started
          _ ->
            Left "Invalid gamestate to postulate an action upon"
      _ ->
        Left "Unwrapping error"


chooseAction :: Gen -> WhichPlayer -> Model -> Scenario -> Maybe Action
chooseAction gen which model scenario
  | modelTurn /= which = Nothing
  | winningEnd model   = Just EndAction
  | otherwise          = Just $ maximumBy comparison $ possibleActions m
  where
    comparison :: Action -> Action -> Ordering
    comparison = comparing $ evalResult . (postulateAction which model gen scenario)
    modelTurn :: Turn
    modelTurn = evalI model getTurn
    m :: Model
    m =
      case which of
        PlayerA ->
          model
        PlayerB ->
          mirror model


winningEnd :: Model -> Bool
winningEnd model
  | evalI model $ handFull PlayerA = False
  | otherwise                      =
    -- If ending the turn now would win, do it! We don't care about heuristics
    -- when we have a sure bet :)
    case fst . runWriter $ resolveAll model Replay.Active.null 0 of
      Ended (Just PlayerA) _ _ _ -> True
      _                          -> False


-- Some cards entail soft advantages/disadvantages that the AI can't handle.
-- We manually set biases for these cards.
biasHand :: Card -> Weight
biasHand c
  | c == Cards.strangeSpore = -9
  | otherwise               = 0
