module Encounter where

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.STM (STM)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Time (NominalDiffTime)
import DeckBuilding (ChosenCharacter (..))
import Room (Room (..))
import Scenario (Scenario (..))
import qualified Start
import Stats.Experience (levelToExperience)
import Stats.Progress (Event (..), Progress (..))
import Util (Gen, modTVar, random)

updateRoomEncounter :: TVar Room -> Progress -> Gen -> STM ()
updateRoomEncounter roomVar progress gen =
  modTVar
    roomVar
    ( \room ->
        room
          { room_scenario =
              encounterScenario progress gen $
                room_scenario room
          }
    )

encounterScenario :: Progress -> Gen -> Scenario -> Scenario
encounterScenario progress gen scenario
  | Set.notMember (EventTutorial 0) events = tutorial0Scenario scenario
  | Set.notMember (EventTutorial 1) events = tutorial1Scenario scenario
  | Set.notMember EventPuzzle events
      && xp >= levelToExperience 9
      && x > 0.95 =
    puzzleScenario scenario
  | otherwise = scenario {scenario_tags = [cs $ show x]}
  where
    events = progress_events progress
    xp = progress_xp progress
    x = random gen

tutorial0Scenario :: Scenario -> Scenario
tutorial0Scenario scenario@Scenario {scenario_progressWin, scenario_progressLoss} =
  scenario
    { scenario_prog = Start.tutorial0Program,
      scenario_roundEndProg = Start.tutorialRoundEndProgram,
      scenario_characterPa = Right . ChosenCharacter $ Nothing,
      scenario_characterPb = Right . ChosenCharacter $ Nothing,
      scenario_timeLimit = noTimeLimit,
      scenario_tags = ["tutorial", "passive"],
      scenario_progressWin =
        scenario_progressWin
          { progress_events = Set.singleton (EventTutorial 0),
            progress_xp = 0
          },
      scenario_progressLoss = scenario_progressLoss {progress_xp = 0}
    }

tutorial1Scenario :: Scenario -> Scenario
tutorial1Scenario scenario@Scenario {scenario_progressWin, scenario_progressLoss} =
  scenario
    { scenario_prog = Start.tutorial1Program,
      scenario_roundEndProg = Start.tutorialRoundEndProgram,
      scenario_characterPa = Right . ChosenCharacter $ Nothing,
      scenario_characterPb = Right . ChosenCharacter $ Nothing,
      scenario_timeLimit = noTimeLimit,
      scenario_tags = ["passive"],
      scenario_progressWin =
        scenario_progressWin
          { progress_events = Set.singleton (EventTutorial 1),
            progress_xp = 0
          },
      scenario_progressLoss = scenario_progressLoss {progress_xp = 0}
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
