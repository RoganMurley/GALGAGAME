{-# LANGUAGE TypeApplications #-}

module Leaderboard.Apps where

import Auth.Schema qualified
import Config (App, runBeam)
import Data.Int (Int32)
import Data.Maybe (catMaybes)
import Database.Beam (QExpr, all_, as_, desc_, filter_, frame_, leftJoin_, limit_, noBounds_, noPartition_, orderBy_, orderPartitionBy_, over_, rank_, references_, runSelectReturningList, runSelectReturningOne, select, val_, withWindow_, (==.))
import Leaderboard.Leaderboard (Leaderboard (..))
import Leaderboard.Leaderboard qualified as Leaderboard
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Schema (StatsT (..))
import User.User qualified as User

type Rank = Int32

load :: App Leaderboard
load = do
  result <-
    runBeam $
      runSelectReturningList $
        select $
          withWindow_
            (\(stats, _) -> frame_ (noPartition_ :: Maybe (QExpr be s Integer)) (orderPartitionBy_ (desc_ $ statsExperience stats)) noBounds_)
            (\row window -> (row, as_ @Int32 rank_ `over_` window))
            ( do
                stats <- limit_ 10 $ orderBy_ (desc_ . statsExperience) $ all_ $ stats galgagameDb
                user <- leftJoin_ (all_ (users galgagameDb)) (\user -> statsUser stats `references_` user)
                pure (stats, user)
            )
  return . Leaderboard $ catMaybes $ Leaderboard.entryFromDb <$> result

loadUserEntry :: User.User -> App (Maybe Leaderboard.Entry)
loadUserEntry user = do
  result <-
    case User.getUserId user of
      Just userId ->
        runBeam $
          runSelectReturningOne $
            select $
              filter_
                (\(row, _) -> statsUser row ==. val_ (Auth.Schema.UserId userId))
                ( withWindow_
                    (\row -> frame_ (noPartition_ :: Maybe (QExpr be s Integer)) (orderPartitionBy_ (desc_ $ statsExperience row)) noBounds_)
                    (\row window -> (row, as_ @Int32 rank_ `over_` window))
                    (all_ $ stats galgagameDb)
                )
      Nothing ->
        return Nothing
  return $ Leaderboard.entryFromStats user <$> result

loadWithMe :: User.User -> Maybe Leaderboard -> App Leaderboard
loadWithMe user mLeaderboard = do
  mEntry <- loadUserEntry user
  result <- maybe load return mLeaderboard
  case mEntry of
    Just entry ->
      return $ Leaderboard.hydrateWithMe entry result
    Nothing ->
      return result