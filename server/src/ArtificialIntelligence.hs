module ArtificialIntelligence where

import qualified Card
import qualified Cards
import Control.Monad.Freer (reinterpret)
import Control.Monad.Trans.Writer (runWriter)
import DSL.Alpha
import DSL.Beta (alphaI)
import Data.List (maximumBy)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.String.Conversions (cs)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GameCommand (GameCommand (..), resolveAll, update)
import GameState
import HandCard (HandCard (..))
import qualified HandCard
import Mirror (mirror)
import Model
import Player (WhichPlayer (..), other)
import Scenario (Scenario (..))
import qualified Stack
import StackCard (StackCard)
import qualified StackCard
import Util (Err, Gen, maybeToEither)

type Weight = Int

data Weightings = Weightings
  { weightings_life :: Weight,
    weightings_hand :: Weight
  }
  deriving (Show)

data Action = PlayAction Int | EndAction
  deriving (Show)

evalResult :: WhichPlayer -> Weightings -> Weightings -> Either Err PlayState -> Weight
evalResult w weightPa weightPb (Right state) = evalState w weightPa weightPb state
evalResult _ _ _ (Left err) = error (cs err)

evalState :: WhichPlayer -> Weightings -> Weightings -> PlayState -> Weight
evalState w _ _ (Ended (Just winner) _ _ _) = if winner == w then 100 else -200
evalState _ _ _ (Ended Nothing _ _ _) = 50
evalState w weightPa weightPb (Playing playing) = evalModel model
  where
    model :: Model
    model = playing_model playing
    evalModel :: Model -> Weight
    evalModel m
      | isJust . (`Stack.get` 1) $ evalI m getStack =
        evalState w weightPa weightPb . fst . runWriter $ resolveAll $ playingFromModel m
      | otherwise =
        evalPlayer w m - evalPlayer (other w) m
    evalPlayer :: WhichPlayer -> Model -> Weight
    evalPlayer which m =
      evalI m $ do
        life <- getLife which
        hand <- getHand which
        return (lifeWeight which * life + handWeight which * length hand + (sum . fmap biasHand $ hand))
    lifeWeight :: WhichPlayer -> Weight
    lifeWeight PlayerA = weightings_life weightPa
    lifeWeight PlayerB = weightings_life weightPb
    handWeight :: WhichPlayer -> Weight
    handWeight PlayerA = weightings_hand weightPa
    handWeight PlayerB = weightings_hand weightPb

toCommand :: Action -> GameCommand
toCommand (PlayAction i) = PlayCard i
toCommand EndAction = EndTurn

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
    xs = [x | x <- [0 .. maxHandLength], x < handLength]
    endAction :: [Action]
    endAction
      | handLength == maxHandLength = []
      | otherwise = [EndAction]

postulateAction :: WhichPlayer -> Model -> Gen -> Scenario -> Action -> Either Err PlayState
postulateAction which model gen scenario action =
  let command = toCommand action :: GameCommand
      state = Started . Playing $ playingFromModel (modI model $ setGen gen) :: GameState
   in update command which state scenario (Nothing, Nothing) (posixSecondsToUTCTime 0)
        >>= maybeToEither "Gamestate not returned from postulation" . fst
        >>= maybeToEither "Gamestate is not a playing state" . playStateFromGameState
        >>= Right . mapModelPlayState (`modI` reinterpret alphaI (scenario_roundEndProg scenario)) -- Account for the draw from the start of the next round.

chooseAction :: Gen -> WhichPlayer -> Model -> Scenario -> Maybe Action
chooseAction gen which rawModel scenario
  | turn /= which = Nothing
  | "passive" `elem` scenario_tags scenario = chooseActionPassive which model
  | winningEnd which model = Just EndAction
  | otherwise = Just $ maximumBy comparison $ possibleActions which model
  where
    model :: Model
    model = getPerceived which rawModel
    turn :: Turn
    turn = evalI model getTurn
    weightPa :: Weightings
    weightPa = Weightings {weightings_hand = 7, weightings_life = 1}
    weightPb :: Weightings
    weightPb
      | "aggressive" `elem` scenario_tags scenario = Weightings {weightings_hand = 3, weightings_life = 1}
      | "save-only" `elem` scenario_tags scenario = Weightings {weightings_hand = 1000, weightings_life = 1}
      | otherwise = Weightings {weightings_hand = 7, weightings_life = 1}
    comparison :: Action -> Action -> Ordering
    comparison = comparing $ evalResult which weightPa weightPb . postulateAction which model gen scenario

winningEnd :: WhichPlayer -> Model -> Bool
winningEnd which model
  | evalI model $ handFull which = False
  | otherwise =
    -- If ending the turn now would win, do it! We don't care about heuristics
    -- when we have a sure bet :)
    case fst . runWriter $ resolveAll (playingFromModel model) of
      Ended (Just winner) _ _ _ -> winner == which
      _ -> False

-- Some cards entail soft advantages/disadvantages that the AI can't handle.
-- We manually set biases for these cards.
biasHand :: HandCard -> Weight
biasHand (HandCard c) = if c == Cards.strangeSpore then -7 else 0
biasHand (KnownHandCard c) = biasHand (HandCard c) - 2

chooseActionPassive :: WhichPlayer -> Model -> Maybe Action
chooseActionPassive which model =
  let hand = evalI model $ getHand which
   in if length hand == maxHandLength
        then Just (PlayAction 0)
        else Just EndAction

getPerceived :: WhichPlayer -> Model -> Model
getPerceived w model =
  model
    { model_stack = fmap perceiveStackCard <$> model_stack model,
      model_pa = perceivePlayerModel $ model_pa model,
      model_pb = perceivePlayerModel $ model_pb model
    }
  where
    -- If we own the stack card, we want to know its true effect.
    -- If we don't own it, we perceive the fake effect if there is one.
    -- This lets the computer get tricked by trick cards.
    perceiveStackCard :: StackCard -> StackCard
    perceiveStackCard sc = if StackCard.isOwner w sc then sc else StackCard.cardMap Card.realiseFakeEff sc
    -- If a card is in our hand, we want to perceive it as a trick card.
    -- This makes the CPU bluff a lot.
    perceivePlayerModel :: PlayerModel -> PlayerModel
    perceivePlayerModel pm =
      pm
        { pmodel_hand = HandCard.cardMap Card.realiseFakeEff <$> pmodel_hand pm,
          pmodel_deck = Card.realiseFakeEff <$> pmodel_deck pm
        }
