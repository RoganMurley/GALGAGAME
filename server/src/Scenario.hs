module Scenario where

import Characters (Character)
import Model (Turn)

data Scenario = Scenario
  { scenario_charactersPa :: Maybe (Character, Character, Character)
  , scenario_charactersPb :: Maybe (Character, Character, Character)
  , scenario_turn         :: Turn
  } deriving (Show)
