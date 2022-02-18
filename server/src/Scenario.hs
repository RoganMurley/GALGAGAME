module Scenario where

import qualified DSL.Beta as Beta
import Data.Time.Clock (NominalDiffTime)
import DeckBuilding (ChosenCharacter, UnchosenCharacter)
import Model (Turn)
import Stats.Experience (Experience)

data Scenario = Scenario
  { scenario_characterPa :: Either UnchosenCharacter ChosenCharacter,
    scenario_characterPb :: Either UnchosenCharacter ChosenCharacter,
    scenario_turn :: Turn,
    scenario_prog :: Beta.Program (),
    scenario_xpWin :: Experience,
    scenario_xpLoss :: Experience,
    scenario_timeLimit :: NominalDiffTime
  }

instance Show Scenario where
  show (Scenario characterPa characterPb turn _ xpWin xpLoss timeLimit) =
    "Scenario: " ++ show (characterPa, characterPb, turn, xpWin, xpLoss, timeLimit)
