{-# LANGUAGE TypeApplications #-}

module Leaderboard.Apps where

import Auth.Schema (User)
import Config (App, runBeam)
import Data.Int (Int32)
import Data.Maybe (catMaybes)
import Database.Beam (all_, as_, desc_, frame_, leftJoin_, limit_, noBounds_, noPartition_, orderBy_, orderPartitionBy_, over_, rank_, references_, runSelectReturningList, select, withWindow_)
import Leaderboard.Leaderboard (Leaderboard (..), entryFromDb, hydrateIsMe)
import qualified Leaderboard.Leaderboard as Leaderboard
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Schema (Stats, StatsT (..))
import qualified User

type Rank = Int32

load :: App Leaderboard
load = do
  result <-
    runBeam $
      runSelectReturningList $
        select $
          withWindow_
            (\(stats, user) -> frame_ noPartition_ (orderPartitionBy_ (desc_ $ statsExperience stats)) noBounds_)
            (\row window -> (row, as_ @Int32 rank_ `over_` window))
            ( do
                stats <- limit_ 10 $ orderBy_ (desc_ . statsExperience) $ all_ $ stats galgagameDb
                user <- leftJoin_ (all_ (users galgagameDb)) (\user -> statsUser stats `references_` user)
                pure (stats, user)
            )
  return . Leaderboard $ catMaybes $ entryFromDb <$> result

-- loadRank :: App Leaderboard
-- loadRank = do
--   result <-
--     runBeam $
--       runSelectReturningList $
--         select $
--           withWindow_
--             (\(stats, user) -> frame_ noPartition_ (orderPartitionBy_ (desc_ $ statsExperience stats)) noBounds_)
--             (\row window -> (row, as_ @Int32 rank_ `over_` window))
--             ( do
--                 stats <- limit_ 10 $ orderBy_ (desc_ . statsExperience) $ all_ $ stats galgagameDb
--                 user <- leftJoin_ (all_ (users galgagameDb)) (\user -> statsUser stats `references_` user)
--                 pure (stats, user)
--             )
--   return . Leaderboard $ catMaybes $ entryFromDb <$> result

loadWithMe :: User.User -> App Leaderboard
loadWithMe user = hydrateIsMe user <$> load