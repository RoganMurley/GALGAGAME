module Scenario where

import Characters (Character)
import Model (Turn)

import qualified DSL.Beta as Beta

data Scenario = Scenario
  { scenario_charactersPa :: Maybe (Character, Character, Character)
  , scenario_charactersPb :: Maybe (Character, Character, Character)
  , scenario_turn         :: Turn
  , scenario_prog         :: Beta.Program ()
  }

instance Show Scenario where
  show (Scenario charactersPa charactersPb turn _) =
    "Scenario: { " ++ show charactersPa ++ " , " ++ show charactersPb  ++ " , " ++ show turn ++ " }"
