module Stats.Stats where

import qualified Auth.Schema
import Config (App, runBeam)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import Database.Beam (all_, current_, filter_, insertValues, primaryKey, runInsert, runSelectReturningOne, runUpdate, select, update, val_, (<-.), (==.))
import qualified Database.Beam.Postgres.Full as Postgres
import DeckBuilding (Rune (..), mainRunes)
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Experience (Experience)
import qualified Stats.Schema
import {-# SOURCE #-} User (User (..))

load :: User -> App Experience
load (User user _) = do
  let userId = Auth.Schema.userId user :: Int64
  result <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> Stats.Schema.statsUser row ==. val_ (Auth.Schema.UserId userId)) $
            all_ $ stats galgagameDb
  return $ maybe 0 Stats.Schema.statsExperience result
load (GuestUser cid _) = loadByCid $ Just cid
load _ = return 0

loadByCid :: Maybe Text -> App Experience
loadByCid (Just cid) = do
  result <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> Stats.Schema.statsguestCid row ==. val_ cid) $
            all_ $ statsguest galgagameDb
  return $ maybe 0 Stats.Schema.statsguestExperience result
loadByCid Nothing = return 0

increase :: User -> Experience -> App ()
increase (User user _) xp = do
  let userId = Auth.Schema.userId user :: Int64
  runBeam $
    runUpdate $
      update
        (stats galgagameDb)
        (\row -> Stats.Schema.statsExperience row <-. current_ (Stats.Schema.statsExperience row) + val_ xp)
        (\row -> Stats.Schema.statsUser row ==. val_ (Auth.Schema.UserId userId))
increase (GuestUser cid _) xp = do
  let val = Stats.Schema.Statsguest cid xp
  runBeam $
    runInsert $
      Postgres.insertOnConflict
        (statsguest galgagameDb)
        (insertValues [val])
        (Postgres.conflictingFields primaryKey)
        ( Postgres.onConflictUpdateSet
            ( \row _ ->
                Stats.Schema.statsguestExperience row
                  <-. current_ (Stats.Schema.statsguestExperience row) + val_ xp
            )
        )
increase _ _ = return ()

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
