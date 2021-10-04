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
import Player (WhichPlayer(..), other)
import Scenario (Scenario(..))
import Stack (diasporaLength)
import Util (Err, Gen)

import qualified Replay.Active
import qualified Cards


type Weight = Int
data Action = PlayAction Int | EndAction
  deriving (Show)


evalResult :: WhichPlayer -> Either Err PlayState -> Weight
evalResult w (Right state) = evalState w state
evalResult _ (Left err)    = error (cs err)


evalState :: WhichPlayer -> PlayState -> Weight
evalState w (Ended (Just winner) _ _ _)  = if winner == w then 100 else -200
evalState _ (Ended Nothing _  _ _)       = 50
evalState w (Playing model _)            = evalModel model
  where
    evalModel :: Model -> Weight
    evalModel m
      | (diasporaLength $ evalI m $ getStack) > 0 =
        (evalState w) . fst . runWriter $ resolveAll m Replay.Active.null 0
      | otherwise =
          (evalPlayer w m) - (evalPlayer (other w) m)
    evalPlayer :: WhichPlayer -> Model -> Weight
    evalPlayer which m =
      evalI m $ do
        life <- getLife which
        hand <- getHand which
        return (life + 7 * (length hand) + (sum . (fmap biasHand) $ hand))


toCommand :: Action -> GameCommand
toCommand (PlayAction i) = PlayCard i
toCommand EndAction      = EndTurn


possibleActions :: WhichPlayer -> Model -> [Action]
possibleActions which model = endAction ++ (PlayAction <$> xs)
  where
    m :: Model
    m =
      case which of
        PlayerA ->
          model
        PlayerB ->
          mirror model
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
  | turn /= which          = Nothing
  | winningEnd which model = Just EndAction
  | otherwise              = Just $ maximumBy comparison $ possibleActions which model
  where
    turn :: Turn
    turn = evalI model getTurn
    comparison :: Action -> Action -> Ordering
    comparison = comparing $ (evalResult which) . (postulateAction which model gen scenario)


winningEnd :: WhichPlayer -> Model -> Bool
winningEnd which model
  | evalI model $ handFull which = False
  | otherwise                      =
    -- If ending the turn now would win, do it! We don't care about heuristics
    -- when we have a sure bet :)
    case fst . runWriter $ resolveAll model Replay.Active.null 0 of
      Ended (Just winner) _ _ _  -> winner == which
      _                          -> False


-- Some cards entail soft advantages/disadvantages that the AI can't handle.
-- We manually set biases for these cards.
biasHand :: Card -> Weight
biasHand c
  | c == Cards.strangeSpore = -9
  | otherwise               = 0
