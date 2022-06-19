module Encounter where

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.STM (STM)
import qualified Data.Set as Set
import Data.Time (NominalDiffTime)
import DeckBuilding (Character (..), ChosenCharacter (..), Rune (..), UnchosenCharacter (..), blazeRune, mirrorRune, heavenRune, shroomRune, tideRune, mirrorRune, alchemyRune, emptyRune, dualityRune, seerRune, feverRune, morphRune, bloodRune, glassRune, myriadRune)
import Room (Room (..))
import Scenario (Scenario (..))
import qualified Start
-- import Stats.Experience (levelToExperience)
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
  | Set.notMember mirrorRune unlocks && xp >= rune_xp mirrorRune = unlockMirrorScenario scenario
  | Set.notMember alchemyRune unlocks && xp >= rune_xp alchemyRune = unlockAlchemyScenario scenario
  | Set.notMember emptyRune unlocks && xp >= rune_xp emptyRune = unlockEmptyScenario scenario
  | Set.notMember dualityRune unlocks && xp >= rune_xp dualityRune = unlockDualityScenario scenario
  | Set.notMember seerRune unlocks && xp >= rune_xp seerRune = unlockSeerScenario scenario
  | Set.notMember feverRune unlocks && xp >= rune_xp feverRune = unlockFeverScenario scenario
  | Set.notMember morphRune unlocks && xp >= rune_xp morphRune = unlockMorphScenario scenario
  | Set.notMember bloodRune unlocks && xp >= rune_xp bloodRune = unlockBloodScenario scenario
  | Set.notMember glassRune unlocks && xp >= rune_xp glassRune = unlockGlassScenario scenario
  | Set.notMember myriadRune unlocks && xp >= rune_xp myriadRune = unlockMyriadScenario scenario
  -- | Set.notMember EventPuzzle events && xp >= levelToExperience 9 = puzzleScenario scenario
  | otherwise = scenario
  where
    xp = progress_xp progress
    events = progress_events progress
    unlocks = progress_unlocks progress

tutorialScenario :: Scenario -> Scenario
tutorialScenario scenario@Scenario {scenario_progressWin} =
  scenario
    { scenario_prog = Start.tutorialProgram,
      scenario_characterPa = Right . ChosenCharacter $ Nothing,
      scenario_characterPb = Right . ChosenCharacter $ Nothing,
      scenario_timeLimit = noTimeLimit,
      scenario_tags = ["tutorial"],
      scenario_progressWin =
        scenario_progressWin
          { progress_events = Set.singleton EventTutorial
          }
    }

unlockMirrorScenario :: Scenario -> Scenario
unlockMirrorScenario scenario@Scenario {scenario_progressWin} =
  scenario
    { scenario_characterPa = Left . UnchosenCharacter $ Nothing,
      scenario_characterPb =
        Right . ChosenCharacter $
          Just $
            Character
              (Left (mirrorRune, tideRune, heavenRune))
              50,
      scenario_progressWin =
        scenario_progressWin
          { progress_unlocks = Set.singleton mirrorRune
          }
    }

unlockAlchemyScenario :: Scenario -> Scenario
unlockAlchemyScenario scenario@Scenario {scenario_progressWin} =
  scenario
    { scenario_characterPa = Left . UnchosenCharacter $ Nothing,
      scenario_characterPb =
        Right . ChosenCharacter $
          Just $
            Character
              (Left (alchemyRune, mirrorRune, shroomRune))
              50,
      scenario_progressWin =
        scenario_progressWin
          { progress_unlocks = Set.singleton alchemyRune
          }
    }

unlockEmptyScenario :: Scenario -> Scenario
unlockEmptyScenario scenario@Scenario {scenario_progressWin} =
  scenario
    { scenario_characterPa = Left . UnchosenCharacter $ Nothing,
      scenario_characterPb =
        Right . ChosenCharacter $
          Just $
            Character
              (Left (emptyRune, mirrorRune, tideRune))
              50,
      scenario_progressWin =
        scenario_progressWin
          { progress_unlocks = Set.singleton emptyRune
          }
    }

unlockDualityScenario :: Scenario -> Scenario
unlockDualityScenario scenario@Scenario {scenario_progressWin} =
  scenario
    { scenario_characterPa = Left . UnchosenCharacter $ Nothing,
      scenario_characterPb =
        Right . ChosenCharacter $
          Just $
            Character
              (Left (dualityRune, mirrorRune, tideRune))
              50,
      scenario_progressWin =
        scenario_progressWin
          { progress_unlocks = Set.singleton dualityRune
          }
    }

unlockSeerScenario :: Scenario -> Scenario
unlockSeerScenario scenario@Scenario {scenario_progressWin} =
  scenario
    { scenario_characterPa = Left . UnchosenCharacter $ Nothing,
      scenario_characterPb =
        Right . ChosenCharacter $
          Just $
            Character
              (Left (seerRune, shroomRune, heavenRune))
              50,
      scenario_progressWin =
        scenario_progressWin
          { progress_unlocks = Set.singleton seerRune
          }
    }

unlockFeverScenario :: Scenario -> Scenario
unlockFeverScenario scenario@Scenario {scenario_progressWin} =
  scenario
    { scenario_characterPa = Left . UnchosenCharacter $ Nothing,
      scenario_characterPb =
        Right . ChosenCharacter $
          Just $
            Character
              (Left (feverRune, dualityRune, heavenRune))
              50,
      scenario_progressWin =
        scenario_progressWin
          { progress_unlocks = Set.singleton feverRune
          }
    }

unlockMorphScenario :: Scenario -> Scenario
unlockMorphScenario scenario@Scenario {scenario_progressWin} =
  scenario
    { scenario_characterPa = Left . UnchosenCharacter $ Nothing,
      scenario_characterPb =
        Right . ChosenCharacter $
          Just $
            Character
              (Left (morphRune, dualityRune, emptyRune))
              50,
      scenario_progressWin =
        scenario_progressWin
          { progress_unlocks = Set.singleton morphRune
          }
    }

unlockBloodScenario :: Scenario -> Scenario
unlockBloodScenario scenario@Scenario {scenario_progressWin} =
  scenario
    { scenario_characterPa = Left . UnchosenCharacter $ Nothing,
      scenario_characterPb =
        Right . ChosenCharacter $
          Just $
            Character
              (Left (bloodRune, blazeRune, shroomRune))
              50,
      scenario_progressWin =
        scenario_progressWin
          { progress_unlocks = Set.singleton bloodRune
          }
    }

unlockGlassScenario :: Scenario -> Scenario
unlockGlassScenario scenario@Scenario {scenario_progressWin} =
  scenario
    { scenario_characterPa = Left . UnchosenCharacter $ Nothing,
      scenario_characterPb =
        Right . ChosenCharacter $
          Just $
            Character
              (Left (glassRune, seerRune, feverRune))
              50,
      scenario_progressWin =
        scenario_progressWin
          { progress_unlocks = Set.singleton glassRune
          }
    }

unlockMyriadScenario :: Scenario -> Scenario
unlockMyriadScenario scenario@Scenario {scenario_progressWin} =
  scenario
    { scenario_characterPa = Left . UnchosenCharacter $ Nothing,
      scenario_characterPb =
        Right . ChosenCharacter $
          Just $
            Character
              (Left (myriadRune, glassRune, mirrorRune))
              50,
      scenario_progressWin =
        scenario_progressWin
          { progress_unlocks = Set.singleton myriadRune
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
