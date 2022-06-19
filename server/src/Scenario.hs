module Scenario where

import qualified DSL.Beta as Beta
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import DeckBuilding (ChosenCharacter, UnchosenCharacter)
import Model (Turn)
import Stats.Progress (Progress)

data Scenario = Scenario
  { scenario_characterPa :: Either UnchosenCharacter ChosenCharacter,
    scenario_characterPb :: Either UnchosenCharacter ChosenCharacter,
    scenario_turn :: Turn,
    scenario_prog :: Beta.Program (),
    scenario_progressWin :: Progress,
    scenario_progressLoss :: Progress,
    scenario_timeLimit :: NominalDiffTime,
    scenario_tags :: [Text]
  }

instance Show Scenario where
  show (Scenario characterPa characterPb turn _ progressWin progressLoss timeLimit tags) =
    "Scenario: " ++ show (characterPa, characterPb, turn, progressWin, progressLoss, timeLimit, tags)
