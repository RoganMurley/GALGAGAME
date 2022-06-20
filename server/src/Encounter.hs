module Encounter where

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.STM (STM)
import qualified Data.Set as Set
import Data.Time (NominalDiffTime)
import DeckBuilding (ChosenCharacter (..))
import Room (Room (..))
import Scenario (Scenario (..))
import qualified Start
import Stats.Progress (Event (..), Progress (..))
import Util (modTVar)

updateRoomEncounter :: TVar Room -> Progress -> STM ()
updateRoomEncounter roomVar progress =
  modTVar
    roomVar
    ( \room ->
        room
          { room_scenario =
              encounterScenario progress $
                room_scenario room
          }
    )

encounterScenario :: Progress -> Scenario -> Scenario
encounterScenario progress scenario
  | Set.notMember EventTutorial events = tutorialScenario scenario
  -- | Set.notMember EventPuzzle events && xp >= levelToExperience 9 = puzzleScenario scenario
  | otherwise = scenario
  where
    events = progress_events progress

tutorialScenario :: Scenario -> Scenario
tutorialScenario scenario@Scenario {scenario_progressWin} =
  scenario
    { scenario_prog = Start.tutorialProgram,
      scenario_roundEndProg = Start.tutorialRoundEndProgram,
      scenario_characterPa = Right . ChosenCharacter $ Nothing,
      scenario_characterPb = Right . ChosenCharacter $ Nothing,
      scenario_timeLimit = noTimeLimit,
      scenario_tags = ["tutorial"],
      scenario_progressWin =
        scenario_progressWin
          { progress_events = Set.singleton EventTutorial
          }
    }

puzzleScenario :: Scenario -> Scenario
puzzleScenario scenario@Scenario {scenario_progressWin} =
  scenario
    { scenario_prog = Start.puzzle,
      scenario_characterPa = Right . ChosenCharacter $ Nothing,
      scenario_characterPb = Right . ChosenCharacter $ Nothing,
      scenario_timeLimit = noTimeLimit,
      scenario_tags = ["puzzle"],
      scenario_progressWin =
        scenario_progressWin
          { progress_events = Set.singleton EventPuzzle
          }
    }

noTimeLimit :: NominalDiffTime
noTimeLimit = 9999999999999999999
