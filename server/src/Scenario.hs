module Scenario where

import Characters (Character)
import Model (Turn)
import Stats.Stats (Experience)

import qualified DSL.Beta as Beta

data Scenario = Scenario
  { scenario_charactersPa :: Maybe (Character, Character, Character)
  , scenario_charactersPb :: Maybe (Character, Character, Character)
  , scenario_turn         :: Turn
  , scenario_prog         :: Beta.Program ()
  , scenario_xpWin        :: Experience
  , scenario_xpLoss       :: Experience
  }

instance Show Scenario where
  show (Scenario charactersPa charactersPb turn _ xpWin xpLoss) =
    "Scenario: " ++ show (charactersPa, charactersPb, turn, xpWin, xpLoss)
