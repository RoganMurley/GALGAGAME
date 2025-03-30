module League.Apps where

import Auth.Schema qualified
import Config (App, runBeam)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Database.Beam (all_, default_, filter_, insert, insertExpressions, runInsert, runSelectReturningOne, select, val_, (==.))
import League.Schema
import Schema (GalgagameDb (..), galgagameDb)

saveLeague :: Int64 -> App ()
saveLeague userId =
  runBeam $
    runInsert $
      insert (league galgagameDb) $
        insertExpressions
          [ League.Schema.League
              default_
              (val_ $ Auth.Schema.UserId $ Just userId)
          ]

checkLeague :: Int64 -> App Bool
checkLeague userId = do
  result <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> League.Schema.leagueUser row ==. val_ (Auth.Schema.UserId (Just userId))) $
            all_ $
              league galgagameDb
  return $ isJust result
