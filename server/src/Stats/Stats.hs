module Stats.Stats where

import qualified Auth.Schema
import Config (App, runBeam, runRedis)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Beam (all_, current_, filter_, runSelectReturningOne, runUpdate, select, update, val_, (<-.), (==.))
import qualified Database.Redis as R
import DeckBuilding (Rune (..), mainRunes)
import qualified Log
import Safe (readMay)
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Experience (Experience)
import qualified Stats.Schema
import {-# SOURCE #-} User (User (..))

load :: User -> App Experience
load (User user _) = loadUser $ Auth.Schema.userUsername user
load (GuestUser cid _) = loadGuest cid
load _ = return 0

loadUser :: Text -> App Experience
loadUser username = do
  result <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> Stats.Schema.statsUser row ==. val_ (Auth.Schema.UserId username)) $
            all_ $ stats galgagameDb
  return $ maybe 0 Stats.Schema.statsExperience result

loadGuest :: Text -> App Experience
loadGuest cid = do
  let key = "guestStats__" <> cid
  result <- runRedis (R.get (cs key))
  case result of
    Left err -> do
      Log.error $ "loadGuest failed with " <> cs (show err)
      return 0
    Right got ->
      return $ fromMaybe 0 (got >>= readMay . cs)

increase :: User -> Experience -> App ()
increase (User user _) xp = increaseUser (Auth.Schema.userUsername user) xp
increase (GuestUser cid _) xp = increaseGuest cid xp
increase _ _ = return ()

increaseUser :: Text -> Experience -> App ()
increaseUser username xp = do
  runBeam $
    runUpdate $
      update
        (stats galgagameDb)
        (\row -> Stats.Schema.statsExperience row <-. current_ (Stats.Schema.statsExperience row) + val_ xp)
        (\row -> Stats.Schema.statsUser row ==. val_ (Auth.Schema.UserId username))

increaseGuest :: Text -> Experience -> App ()
increaseGuest cid xp = do
  let key = "guestStats__" <> cid
  _ <-
    runRedis $
      R.set (cs key) (cs $ show xp)
  return ()

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
