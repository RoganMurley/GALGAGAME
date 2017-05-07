module ArtificalIntelligence where

import Control.Monad.Writer (runWriter)
import Data.List (maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import GameState
import Model
import Player (WhichPlayer(..))
import Util (fromRight)


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
          (getLife PlayerA m)
        - (getLife PlayerB m)
        + (length . (getHand PlayerA) $ m) * 7
        - (length . (getHand PlayerB) $ m) * 7


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
    endAction =
      if handLength == maxHandLength
        then []
        else [EndAction]


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
  | otherwise =
    Just $ maximumBy comparison $ possibleActions model
  where
    comparison :: Action -> Action -> Ordering
    comparison = comparing (evalState . (postulateAction model))
