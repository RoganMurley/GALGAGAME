{-# LANGUAGE DeriveGeneric #-}
module World.World where

import Cards (cardsByName)
import Card (cardName)
import Config (App)
import Control.Concurrent.STM.TVar (TVar)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Life (initMaxLife)
import Model (Deck)
import Player (WhichPlayer(..))
import Safe (readMay)
import Scenario (Scenario(..))
import Start (startProgram)
import Util (breakAt)

import World.Decision (Decision(..), decisionFromId)
import World.Encounter (Encounter(..), newEncounter)
import World.Pos (Pos)
import World.WorldKey (WorldKey)
import World.WorldProgress (WorldProgress(..), nextGen)
import World.WorldTree (Edge(..), getAdjEdges, getEdgePositions, getPosition, lockedEdgePositions, mainKeys)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified DeckBuilding
import qualified Server


data World = World
  { world_encounters    :: [Encounter]
  , world_others        :: [Pos]
  , world_edgePositions :: [(Pos, Pos)]
  , world_visited       :: [Pos]
  , world_visitedEdges  :: [(Pos, Pos)]
  , world_lockedEdges   :: [(Pos, Pos)]
  , world_decision      :: Maybe Decision
  }
  deriving (Eq, Show)


getWorld :: TVar Server.State -> WorldProgress -> App World
getWorld _ progress = do
  let key = worldprogress_key progress
  let adjEdges = getAdjEdges key
  let visitedKeys = worldprogress_visited progress
  let edges = filter (\Edge{ edge_key } -> not $ Set.member edge_key visitedKeys) adjEdges
  let edgeKeys = Set.fromList $ edge_key <$> edges :: Set WorldKey
  let otherKeys = Set.difference mainKeys (Set.union edgeKeys visitedKeys) :: Set WorldKey
  let others = getPosition <$> Set.toList otherKeys :: [Pos]
  encounters <- mapM (newEncounter progress) edges
  return $
    World
    { world_encounters    = encounters
    , world_others        = others
    , world_edgePositions = getEdgePositions key edgeKeys
    , world_visited       = getPosition <$> Set.toList visitedKeys
    , world_visitedEdges  = worldprogress_visitedEdges progress
    , world_lockedEdges   = lockedEdgePositions
    , world_decision      = worldprogress_decisionId progress >>= decisionFromId
    }


instance ToJSON World where
  toJSON (World{ world_decision, world_encounters, world_others, world_edgePositions, world_lockedEdges, world_visited, world_visitedEdges }) =
    object [
      "encounters"   .= toJSON world_encounters
    , "others"       .= toJSON world_others
    , "edges"        .= toJSON world_edgePositions
    , "visited"      .= toJSON world_visited
    , "visitedEdges" .= toJSON world_visitedEdges
    , "lockedEdges"  .= toJSON world_lockedEdges
    , "decision"     .= toJSON world_decision
    ]


data WorldRequest
  = JoinEncounter Text
  | EncounterDecision Int
  deriving (Eq, Show)


parseRequest :: Text -> Maybe WorldRequest
parseRequest msg =
  let
    (command, content) = breakAt ":" msg :: (Text, Text)
  in
    case command of
      "joinEncounter" ->
        Just . JoinEncounter $ content
      "encounterDecision" ->
        case readMay $ cs content of
          Just choice ->
            Just . EncounterDecision $ choice
          Nothing ->
            Nothing
      _ ->
        Nothing


makeScenario :: WorldProgress -> Encounter -> Scenario
makeScenario (WorldProgress{ worldprogress_deck }) encounter =
  Scenario {
    scenario_turn = PlayerA
  , scenario_characterPa = characterPa
  , scenario_characterPb = characterPb
  , scenario_prog = startProgram PlayerA
  , scenario_xpWin = 100
  , scenario_xpLoss = 70
  , scenario_reward = Nothing
  }
  where
    Encounter{ encounter_cardNames, encounter_life, encounter_name } = encounter
    characterPa :: Maybe DeckBuilding.Character
    characterPa =
      case encounter_cardNames of
        Just cardNames ->
          Just $
            DeckBuilding.Character
              encounter_name
              ""
              (Right $ deckFromCardNames cardNames)
              encounter_life
        Nothing ->
          Nothing
    characterPb :: Maybe DeckBuilding.Character
    characterPb =
      Just $
        DeckBuilding.Character
          "Prideful Fool"
          ""
          (Right $ deckFromCardNames worldprogress_deck >>= replicate 3)
          initMaxLife


deckFromCardNames :: [Text] -> Deck
deckFromCardNames names = catMaybes $ (\name -> Map.lookup name cardsByName) <$> names


pvpScenario :: WorldProgress -> Scenario -> Scenario
pvpScenario (WorldProgress{ worldprogress_deck }) scenario =
  scenario { scenario_characterPb =
    Just $
      DeckBuilding.Character
        "Prideful Fool"
        ""
        (Right $ deckFromCardNames worldprogress_deck >>= replicate 3)
        initMaxLife
  }


preEncounterProgress :: Encounter -> WorldProgress -> WorldProgress
preEncounterProgress (Encounter{ encounter_guid }) progress =
  progress { worldprogress_roomId = Just encounter_guid }


postEncounterProgress :: Encounter -> Scenario -> WorldProgress -> WorldProgress
postEncounterProgress encounter scenario progress =
  checkWin $
    WorldProgress
      { worldprogress_key          = encounter_key
      , worldprogress_visited      = Set.insert encounter_key worldprogress_visited
      , worldprogress_visitedEdges = edge : worldprogress_visitedEdges
      , worldprogress_deck         = deck
      , worldprogress_decisionId   = decision_id <$> encounter_decision
      , worldprogress_roomId       = Nothing
      , worldprogress_gen          = nextGen progress
      }
  where
    Encounter{ encounter_decision, encounter_key, encounter_x, encounter_y } = encounter
    WorldProgress{ worldprogress_deck, worldprogress_key, worldprogress_visited, worldprogress_visitedEdges } = progress
    Scenario{ scenario_reward } = scenario
    edge :: (Pos, Pos)
    edge = (getPosition worldprogress_key, (encounter_x, encounter_y))
    deck =
      case scenario_reward of
        Just rewardCards ->
          (cardName <$> rewardCards) ++ worldprogress_deck
        Nothing ->
          worldprogress_deck


checkWin :: WorldProgress -> WorldProgress
checkWin progress =
  if didWin then
    progress { worldprogress_decisionId = Just "complete" }
      else
        progress
  where
    WorldProgress{ worldprogress_key, worldprogress_visited } = progress
    didWin :: Bool
    didWin =
      null $
        filter
          (\Edge{ edge_key } -> not $ Set.member edge_key worldprogress_visited)
          (getAdjEdges worldprogress_key)


encounterFromGuid :: World -> Text -> Maybe Encounter
encounterFromGuid (World{ world_encounters }) guid =
  find
    (\encounter -> encounter_guid encounter == guid)
    world_encounters
