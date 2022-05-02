module Leaderboard.Apps where

import Config (App, runBeam)
import Database.Beam (all_, desc_, limit_, orderBy_, runSelectReturningList, select)
import Leaderboard.Leaderboard (Leaderboard(..), entryFromStats)
import Schema (GalgagameDb (..), galgagameDb)
import qualified Stats.Schema

loadLeaderboard :: App Leaderboard
loadLeaderboard = do
  result <-
    runBeam $
      runSelectReturningList $
        select $
          limit_ 10  $
            orderBy_ (desc_ . Stats.Schema.statsExperience) $
              all_ $
                stats galgagameDb
  return . Leaderboard $ entryFromStats <$> result
