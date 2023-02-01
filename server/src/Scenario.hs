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
    scenario_turn :: Maybe Turn,
    scenario_prog :: Maybe (Text, Text) -> Beta.Program (),
    scenario_roundEndProg :: Beta.Program (),
    scenario_progressWin :: Progress,
    scenario_progressLoss :: Progress,
    scenario_timeLimit :: NominalDiffTime,
    scenario_tags :: [Text]
  }

instance Show Scenario where
  show (Scenario characterPa characterPb turn _ _ progressWin progressLoss timeLimit tags) =
    "Scenario: " ++ show (characterPa, characterPb, turn, progressWin, progressLoss, timeLimit, tags)

data CustomSettings = CustomSettings
  { customsettings_name :: Text,
    customsettings_allowSpectators :: Bool,
    customsettings_startingLife :: Int
  }

parseCustomSettings :: Text -> Either Text CustomSettings
parseCustomSettings _ =
  Right $
    CustomSettings
      { customsettings_name = "abc",
        customsettings_allowSpectators = True,
        customsettings_startingLife = 50
      }

applyCustomSettings :: Maybe CustomSettings -> Scenario -> Scenario
applyCustomSettings Nothing scenario = scenario
applyCustomSettings (Just CustomSettings {}) scenario =
  scenario

markCpu :: Scenario -> Scenario
markCpu scenario@Scenario {scenario_tags} =
  scenario
    { scenario_tags = "cpu" : scenario_tags
    }
