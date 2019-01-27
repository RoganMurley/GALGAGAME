module Scenario where

import Characters (Character)

data Scenario = Scenario
  { scenario_charactersPa :: Maybe (Character, Character, Character)
  , scenario_charactersPb :: Maybe (Character, Character, Character)
  } deriving (Show)
