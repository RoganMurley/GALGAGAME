module Leaderboard.Apps where

import Config (App, runBeam)
import Data.Text (Text)
import Data.String.Conversions (cs)
import Database.Beam (all_, desc_, orderBy_, runSelectReturningList, select)
import Schema (GalgagameDb (..), galgagameDb)
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
  return $ (\row -> (cs . show . Stats.Schema.statsUser $ row, Stats.Schema.statsExperience row)) <$> result
