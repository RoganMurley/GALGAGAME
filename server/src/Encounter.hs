module Encounter where

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.STM (STM)
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
    encounterScenario xp scenario
      | xp < levelToExperience 2 =
        scenario
          { scenario_prog = Start.tutorialProgram,
            scenario_characterPa = Right . ChosenCharacter $ Nothing,
            scenario_characterPb = Right . ChosenCharacter $ Nothing,
            scenario_timeLimit = 9999999999999999999,
            scenario_tags = ["tutorial"]
          }
      | otherwise = scenario
