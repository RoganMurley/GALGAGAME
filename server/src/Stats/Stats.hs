module Stats.Stats where

import qualified Auth.Schema
import Config (App, runBeam)
import Data.Aeson (ToJSON (..), decode, encode, object, (.=))
import Data.Int (Int64)
import Data.Set (Set)
import qualified Data.Set as Set
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

load :: User -> App (Maybe Progress)
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
  return progress
load (GuestUser cid _) = loadByCid $ Just cid
load _ = return . Just $ initialProgress

loadByCid :: Maybe Text -> App (Maybe Progress)
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
  return progress
loadByCid Nothing = return . Just $ initialProgress

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
    statChange_finalExperience :: Experience,
    statChange_newUnlocks :: Set Rune
  }
  deriving (Show, Eq)

instance ToJSON StatChange where
  toJSON
    StatChange
      { statChange_initialExperience,
        statChange_finalExperience,
        statChange_newUnlocks
      } =
      object
        [ "initialExperience" .= statChange_initialExperience,
          "finalExperience" .= statChange_finalExperience,
          "unlocks" .= statChange_newUnlocks
        ]

statChange :: Progress -> Progress -> StatChange
statChange initial final =
  let initialUnlocks = progress_unlocks initial
      finalUnlocks = progress_unlocks final
      newUnlocks = Set.difference finalUnlocks initialUnlocks
   in StatChange
        { statChange_initialExperience = progress_xp initial,
          statChange_finalExperience = progress_xp final,
          statChange_newUnlocks = newUnlocks
        }

hydrateUnlocks :: Progress -> Progress
hydrateUnlocks progress =
  progress {progress_unlocks = Set.union unlocks newUnlocks}
  where
    unlocks = progress_unlocks progress
    xp = progress_xp progress
    newUnlocks =
      Set.fromList $
        filter
          (\rune -> xp >= rune_xp rune)
          mainRunes

isXpChange :: StatChange -> Bool
isXpChange StatChange {statChange_initialExperience, statChange_finalExperience} =
  statChange_initialExperience /= statChange_finalExperience
