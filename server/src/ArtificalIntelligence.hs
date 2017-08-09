module ArtificalIntelligence where

import Control.Monad.Trans.Writer (runWriter)
import Data.List (maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import GameState
import Model
import Player (WhichPlayer(..))
import Util (fromRight)

import qualified Cards


type Weight = Int
data Action = PlayAction Int | EndAction
  deriving (Show)


evalState :: PlayState -> Weight
evalState (Ended (Just PlayerA) _) = 100
evalState (Ended (Just PlayerB) _) = -200
evalState (Ended Nothing  _)       = 50
evalState (Playing model)          = evalModel model
  where
    evalModel :: Model -> Weight
    evalModel m
      | (length . getStack $ m) > 0 =
        evalState . fst . runWriter . resolveAll $ m
      | otherwise =
          (evalPlayer PlayerA m) - (evalPlayer PlayerB m)
    evalPlayer :: WhichPlayer -> Model -> Weight
    evalPlayer which m =
        (getLife which m)
      + (length . (getHand which) $ m) * 7
      + (sum . (fmap biasHand) . (getHand which) $ m)


toCommand :: Action -> GameCommand
toCommand (PlayAction i) = PlayCard i
toCommand EndAction      = EndTurn


possibleActions :: Model -> [Action]
possibleActions m =
  endAction ++ (PlayAction <$> xs)
  where
    handLength :: Int
    handLength = length . (getHand PlayerA) $ m
    xs :: [Int]
    xs = [ x | x <- [0..maxHandLength], x < handLength]
    endAction :: [Action]
    endAction
      | handLength == maxHandLength =
        []
      | otherwise =
        [EndAction]


postulateAction :: Model -> Action -> PlayState
postulateAction model action =
  -- DANGEROUS, WE NEED TO SPLIT UP THE COMMAND STUFF IN GAMESTATE
  (\(Started p) -> p) . fromJust . fst . fromRight $ update command PlayerA state
  where
    command = toCommand action :: GameCommand
    state = Started (Playing model) :: GameState


chooseAction :: Turn -> Model -> Maybe Action
chooseAction turn model
  | getTurn model /= turn =
    Nothing
  | winningEnd model =
    Just EndAction
  | otherwise =
    Just $ maximumBy comparison $ possibleActions model
  where
    comparison :: Action -> Action -> Ordering
    comparison = comparing (evalState . (postulateAction model))


winningEnd :: Model -> Bool
winningEnd m =
  -- If ending the turn now would win, do it! We don't care about heuristics
  -- when we have a sure bet :)
  case fst . runWriter . resolveAll $ m of
    Ended (Just PlayerA) _ ->
      True
    _ ->
      False


-- Some cards entail soft advantages/disadvantages that the AI can't handle.
-- We manually set biases for these cards.
biasHand :: Card -> Weight
biasHand c
  | c == Cards.badApple     = -9
  | c == (Cards.obscured c) = -3
  | c == Cards.hoard        = -6
  | c == Cards.exile        = -3
  | otherwise               = 0
