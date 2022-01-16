module League.Apps where

import qualified Auth.Schema
import Config (App, runBeam)
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.String.Conversions (cs)
import Database.Beam (all_, default_, filter_, insert, insertExpressions, runInsert, runSelectReturningOne, select, val_, (==.))
import League.Schema
import Schema (GalgagameDb (..), galgagameDb)

saveLeague :: ByteString -> App ()
saveLeague username =
  runBeam $
    runInsert $
      insert (league galgagameDb) $
        insertExpressions
          [ League.Schema.League
              default_
              (val_ $ Auth.Schema.UserId $ Just $ cs username)
          ]

checkLeague :: ByteString -> App Bool
checkLeague username = do
  result <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> League.Schema.leagueUser row ==. val_ (Auth.Schema.UserId (Just $ cs username))) $
            all_ $ league galgagameDb
  return $ isJust result