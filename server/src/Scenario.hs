module Scenario where

import DeckBuilding (Character)
import Model (Turn)
import Stats.Stats (Experience)

import qualified DSL.Beta as Beta

data Scenario = Scenario
  { scenario_characterPa :: Maybe Character
  , scenario_characterPb :: Maybe Character
  , scenario_turn        :: Turn
  , scenario_prog        :: Beta.Program ()
  , scenario_xpWin       :: Experience
  , scenario_xpLoss      :: Experience
  }

instance Show Scenario where
  show (Scenario characterPa characterPb turn _ xpWin xpLoss) =
    "Scenario: " ++ show (characterPa, characterPb, turn, xpWin, xpLoss)
