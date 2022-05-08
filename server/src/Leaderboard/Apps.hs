module Leaderboard.Apps where

import Config (App, runBeam)
import Database.Beam (all_, desc_, leftJoin_, limit_, orderBy_, references_, runSelectReturningList, select)
import Leaderboard.Leaderboard (Leaderboard (..), entryFromStats)
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Schema (StatsT (..))
import qualified User

load :: User.User -> App Leaderboard
load gameuser = do
  result <-
    runBeam $
      runSelectReturningList $
        select $ do
          stats <- limit_ 10 $ orderBy_ (desc_ . statsExperience) $ all_ $ stats galgagameDb
          user <- leftJoin_ (all_ (users galgagameDb)) (\user -> statsUser stats `references_` user)
          pure (stats, user)
  return . Leaderboard $ entryFromStats gameuser <$> result
