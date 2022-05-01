module Leaderboard.Apps where

import Config (App, runBeam)
import Data.Text (Text)
import Database.Beam (all_, desc_, limit_, orderBy_, runSelectReturningList, select)
import Schema (GalgagameDb (..), galgagameDb)
import Auth.Schema (PrimaryKey(UserId))
import Stats.Experience (Experience)
import qualified Stats.Schema

loadLeaderboard :: App [(Text, Experience)]
loadLeaderboard = do
  result <-
    runBeam $
      runSelectReturningList $
        select $
          limit_ 10  $
            orderBy_ (desc_ . Stats.Schema.statsExperience) $
              all_ $
                stats galgagameDb
  return $ (\row -> ((\(UserId username) -> username) . Stats.Schema.statsUser $ row, Stats.Schema.statsExperience row)) <$> result
