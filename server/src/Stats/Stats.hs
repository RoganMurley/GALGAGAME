module Stats.Stats where

import qualified Auth.Schema
import Config (App, runBeam)
import Data.Aeson (ToJSON (..), decode, encode, object, (.=))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Beam (all_, filter_, insertValues, primaryKey, runInsert, runSelectReturningOne, runUpdate, select, update, val_, (<-.), (==.))
import qualified Database.Beam.Postgres.Full as Postgres
import DeckBuilding (Rune (..), mainRunes)
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Experience (Experience)
import Stats.Progress (Progress (..), fromPartial, initialProgress)
import qualified Stats.Schema
import {-# SOURCE #-} User (User (..))

load :: User -> App Progress
load (User user _) = do
  let userId = Auth.Schema.userId user :: Int64
  result <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> Stats.Schema.statsUser row ==. val_ (Auth.Schema.UserId userId)) $
            all_ $ stats galgagameDb
  let progress =
        result
          >>= ( \r ->
                  let xp = Stats.Schema.statsExperience r
                      partial =
                        Stats.Schema.statsProgress r
                          >>= return . cs
                          >>= decode
                   in fromPartial <$> partial <*> Just xp
              )
  return $ fromMaybe initialProgress progress
load (GuestUser cid _) = loadByCid $ Just cid
load _ = return initialProgress

loadByCid :: Maybe Text -> App Progress
loadByCid (Just cid) = do
  result <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> Stats.Schema.statsguestCid row ==. val_ cid) $
            all_ $ statsguest galgagameDb
  let progress =
        result
          >>= ( \r ->
                  let xp = Stats.Schema.statsguestExperience r
                      partial =
                        Stats.Schema.statsguestProgress r
                          >>= return . cs
                          >>= decode
                   in fromPartial <$> partial <*> Just xp
              )
  return $ fromMaybe initialProgress progress
loadByCid Nothing = return initialProgress

updateProgress :: User -> Progress -> App ()
updateProgress (User user _) progress = do
  let userId = Auth.Schema.userId user :: Int64
  let xp = progress_xp progress :: Experience
  runBeam $
    runUpdate $
      update
        (stats galgagameDb)
        ( \row ->
            mconcat
              [ Stats.Schema.statsExperience row <-. val_ xp,
                Stats.Schema.statsProgress row <-. val_ (Just . cs $ encode progress)
              ]
        )
        (\row -> Stats.Schema.statsUser row ==. val_ (Auth.Schema.UserId userId))
updateProgress (GuestUser cid _) progress = do
  let xp = progress_xp progress :: Experience
  let encodedProgress = Just . cs $ encode progress
  let val = Stats.Schema.Statsguest cid xp encodedProgress
  runBeam $
    runInsert $
      Postgres.insertOnConflict
        (statsguest galgagameDb)
        (insertValues [val])
        (Postgres.conflictingFields primaryKey)
        ( Postgres.onConflictUpdateSet
            ( \row _ ->
                mconcat
                  [ Stats.Schema.statsguestExperience row <-. val_ xp,
                    Stats.Schema.statsguestProgress row <-. val_ encodedProgress
                  ]
            )
        )
updateProgress _ _ = return ()

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
