module ArtificalIntelligence where

import Control.Monad.Trans.Writer (runWriter)
import Data.List (maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import GameCommand (GameCommand(..), resolveAll, update)
import GameState
import Model
import Player (WhichPlayer(..))
import Util (Gen, fromRight)

import qualified Cards


type Weight = Int
data Action = PlayAction Int | EndAction
  deriving (Show)


evalState :: PlayState -> Weight
evalState (Ended (Just PlayerA) _ _) = 100
evalState (Ended (Just PlayerB) _ _) = -200
evalState (Ended Nothing _ _)        = 50
evalState (Playing model)          = evalModel model
  where
    evalModel :: Model -> Weight
    evalModel m
      | (length $ evalI m $ getStack) > 0 =
        evalState . fst . runWriter . resolveAll $ m
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
possibleActions m =
  endAction ++ (PlayAction <$> xs)
  where
    handLength :: Int
    handLength = length $ evalI m $ getHand PlayerA
    xs :: [Int]
    xs = [ x | x <- [0..maxHandLength], x < handLength]
    endAction :: [Action]
    endAction
      | handLength == maxHandLength = []
      | otherwise = [ EndAction ]


postulateAction :: Model -> Gen -> Action -> PlayState
postulateAction model gen action =
  -- DANGEROUS, WE NEED TO SPLIT UP THE COMMAND STUFF IN GAMESTATE
  (\(Started p) -> p) . fromJust . fst . fromRight $ update command PlayerA state
  where
    command = toCommand action :: GameCommand
    state = Started $ Playing $ modI model $ setGen gen :: GameState


chooseAction :: Gen -> Turn -> Model -> Maybe Action
chooseAction gen turn model
  | modelTurn /= turn = Nothing
  | winningEnd model  = Just EndAction
  | otherwise         = Just $ maximumBy comparison $ possibleActions model
  where
    comparison :: Action -> Action -> Ordering
    comparison = comparing $ evalState . (postulateAction model gen)
    modelTurn :: Turn
    modelTurn = evalI model getTurn


winningEnd :: Model -> Bool
winningEnd model
  | evalI model $ handFull PlayerA = False
  | otherwise                      =
    -- If ending the turn now would win, do it! We don't care about heuristics
    -- when we have a sure bet :)
    case fst . runWriter . resolveAll $ model of
      Ended (Just PlayerA) _ _ ->
        True
      _ ->
        False


-- Some cards entail soft advantages/disadvantages that the AI can't handle.
-- We manually set biases for these cards.
biasHand :: Card -> Weight
biasHand c
  | c == Cards.badApple     = -9
  | otherwise               = 0
