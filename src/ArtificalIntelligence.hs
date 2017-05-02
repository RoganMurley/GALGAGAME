module ArtificalIntelligence where

import Control.Monad.Writer (runWriter)
import Data.List (maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import GameState
import Model
import Util (fromRight)


type Weight = Int
data Action = PlayAction Int | EndAction
  deriving (Show)


evaluateState :: PlayState -> Weight
evaluateState (Playing m)              = evaluateModel m
evaluateState (Ended (Just PlayerA) _) = 100
evaluateState (Ended (Just PlayerB) _) = -200
evaluateState (Ended Nothing  _)       = 50


evaluateModel :: Model -> Weight
evaluateModel m =
  if (length . getStack $ m) > 0 then
    evaluateState . fst . runWriter . resolveAll $ m
      else
        weighting
  where
    weighting :: Weight
    weighting =
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


chooseAction :: Model -> Action
chooseAction model =
  maximumBy comparison (possibleActions model)
  where
    comparison :: Action -> Action -> Ordering
    comparison = comparing (evaluateState . (postulateAction model))
