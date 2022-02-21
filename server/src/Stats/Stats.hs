module Stats.Stats where

import qualified Auth.Schema
import Config (App, runBeam)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.Beam (all_, current_, filter_, runSelectReturningOne, runUpdate, select, update, val_, (<-.), (==.))
import DeckBuilding (Rune (..), mainRunes)
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Experience (Experience)
import qualified Stats.Schema

load :: Text -> App Experience
load username = do
  result <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> Stats.Schema.statsUser row ==. val_ (Auth.Schema.UserId username)) $
            all_ $ stats galgagameDb
  return $ fromMaybe 0 $ Stats.Schema.statsExperience <$> result

increase :: Text -> Experience -> App ()
increase username xp = do
  runBeam $
    runUpdate $
      update
        (stats galgagameDb)
        (\row -> Stats.Schema.statsExperience row <-. current_ (Stats.Schema.statsExperience row) + val_ xp)
        (\row -> Stats.Schema.statsUser row ==. val_ (Auth.Schema.UserId username))

data StatChange = StatChange
  { statChange_initialExperience :: Experience,
    statChange_finalExperience :: Experience
  }
  deriving (Show, Eq)

instance ToJSON StatChange where
  toJSON
    s@StatChange
      { statChange_initialExperience,
        statChange_finalExperience
      } =
      object
        [ "initialExperience" .= statChange_initialExperience,
          "finalExperience" .= statChange_finalExperience,
          "unlocks" .= newUnlocks s
        ]

statChange :: Experience -> Experience -> StatChange
statChange xp delta =
  StatChange
    { statChange_initialExperience = xp,
      statChange_finalExperience = xp + delta
    }

newUnlocks :: StatChange -> [Rune]
newUnlocks StatChange {statChange_initialExperience, statChange_finalExperience} =
  filter (\rune -> statChange_finalExperience >= rune_xp rune) $
    filter (\rune -> statChange_initialExperience < rune_xp rune) mainRunes
