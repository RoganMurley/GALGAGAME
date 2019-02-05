module Scenario where

import Characters (Character)
import ModelDiff (ModelDiff)

data Scenario = Scenario
  { scenario_charactersPa :: Maybe (Character, Character, Character)
  , scenario_charactersPb :: Maybe (Character, Character, Character)
  , scenario_modelDiff    :: ModelDiff
  } deriving (Show)
