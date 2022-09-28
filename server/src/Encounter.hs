module Encounter where

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.STM (STM)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Time (NominalDiffTime)
import DeckBuilding (ChosenCharacter (..), heavenRune)
import Room (Room (..))
import Scenario (Scenario (..))
import qualified Start
import Stats.Experience (levelToExperience)
import Stats.Progress (Progress (..))
import Util (Gen, modTVar, random)
import Player (WhichPlayer(..))

updateRoomEncounter :: TVar Room -> Progress -> Gen -> STM ()
updateRoomEncounter roomVar progress gen =
  modTVar
    roomVar
    ( \room ->
        room
          { room_scenario =
              markCpu $ 
                encounterScenario progress gen $
                  room_scenario room
          }
    )

encounterScenario :: Progress -> Gen -> Scenario -> Scenario
encounterScenario progress gen scenario
  | Set.notMember "tutorial-0" events = tutorial0Scenario scenario
  | Set.notMember "tutorial-1" events = tutorial1Scenario scenario
  -- | Set.notMember "tutorial-2" events = tutorial2Scenario scenario
  -- | Set.notMember "tutorial-3" events = tutorial3Scenario scenario
  | Set.notMember "tutorial-puzzle-morph" events
      && xp >= levelToExperience 9
      && x > 0.95 =
    puzzleScenario scenario
  -- | True = turboScenario scenario
  | otherwise = scenario {scenario_tags = [cs $ show x, "normal"]}
  where
    events = progress_events progress
    xp = progress_xp progress
    x = random gen

tutorial0Scenario :: Scenario -> Scenario
tutorial0Scenario scenario@Scenario {scenario_progressWin, scenario_progressLoss} =
  scenario
    { scenario_prog = Start.tutorial0Program,
      scenario_characterPa = Right . ChosenCharacter $ Nothing,
      scenario_characterPb = Right . ChosenCharacter $ Nothing,
      scenario_timeLimit = noTimeLimit,
      scenario_tags = ["tutorial-0", "aggressive"],
      scenario_progressWin =
        scenario_progressWin
          { progress_events = Set.fromList ["tutorial-0"],
            progress_xp = 20,
            progress_unlocks = Set.fromList [heavenRune]
          },
      scenario_progressLoss = scenario_progressLoss {progress_xp = 0}
    }

tutorial1Scenario :: Scenario -> Scenario
tutorial1Scenario scenario@Scenario {scenario_progressWin, scenario_progressLoss} =
  scenario
    { scenario_prog = Start.tutorial1Program,
      scenario_characterPa = Right . ChosenCharacter $ Nothing,
      scenario_characterPb = Right . ChosenCharacter $ Nothing,
      scenario_timeLimit = noTimeLimit,
      scenario_tags = ["tutorial-1", "save-only"],
      scenario_progressWin =
        scenario_progressWin
          { progress_events = Set.fromList ["tutorial-1"],
            progress_xp = 20
          },
      scenario_progressLoss = scenario_progressLoss {progress_xp = 0}
    }

-- tutorial2Scenario :: Scenario -> Scenario
-- tutorial2Scenario scenario@Scenario {scenario_progressWin, scenario_progressLoss} =
--   scenario
--     { scenario_prog = Start.tutorial2Program,
--       scenario_characterPa = Right . ChosenCharacter $ Nothing,
--       scenario_characterPb = Right . ChosenCharacter $ Nothing,
--       scenario_timeLimit = noTimeLimit,
--       scenario_tags = ["tutorial-2"],
--       scenario_progressWin =
--         scenario_progressWin
--           { progress_events = Set.fromList ["tutorial-2", "tutorial-complete"],
--             progress_xp = 20
--           },
--       scenario_progressLoss = scenario_progressLoss {progress_xp = 0}
--     }

puzzleScenario :: Scenario -> Scenario
puzzleScenario scenario@Scenario {scenario_progressWin} =
  scenario
    { scenario_prog = Start.puzzle,
      scenario_roundEndProg = Start.defeatRoundEndProgram PlayerA,
      scenario_characterPa = Right . ChosenCharacter $ Nothing,
      scenario_characterPb = Right . ChosenCharacter $ Nothing,
      scenario_timeLimit = noTimeLimit,
      scenario_tags = ["puzzle"],
      scenario_progressWin =
        scenario_progressWin
          { progress_events = Set.singleton "tutorial-puzzle-morph"
          }
    }

turboScenario :: Scenario -> Scenario
turboScenario scenario =
  scenario
    { scenario_timeLimit = 10,
      scenario_tags = ["turbo"]
    }

markCpu :: Scenario -> Scenario
markCpu scenario@Scenario {scenario_tags} =
  scenario
    { scenario_tags = "cpu" : scenario_tags}

noTimeLimit :: NominalDiffTime
noTimeLimit = 9999999999999999999
