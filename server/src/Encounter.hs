module Encounter where

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.STM (STM)
import qualified DSL.Beta as Beta
import Data.Text (Text)
import DeckBuilding (ChosenCharacter (..))
import Room (Room (..))
import Scenario (Scenario (..))
import qualified Start
import Stats.Experience (Experience, levelToExperience)
import Util (modTVar)

updateRoomEncounter :: TVar Room -> Experience -> STM ()
updateRoomEncounter roomVar experience =
  modTVar roomVar (\room -> room {room_scenario = encounterScenario experience $ room_scenario room})
  where
    encounterScenario :: Experience -> Scenario -> Scenario
    encounterScenario xp scenario =
      case prog xp of
        Just p ->
          scenario
            { scenario_prog = p,
              scenario_characterPa = Right . ChosenCharacter $ Nothing,
              scenario_characterPb = Right . ChosenCharacter $ Nothing,
              scenario_timeLimit = 9999999999999999999,
              scenario_tags = tags xp
            }
        Nothing ->
          scenario
    prog :: Experience -> Maybe (Beta.Program ())
    prog xp
      | xp < levelToExperience 2 = Just Start.tutorialProgram
      | otherwise = Nothing
    tags :: Experience -> [Text]
    tags xp
      | xp < levelToExperience 2 = ["tutorial"]
      | otherwise = []
