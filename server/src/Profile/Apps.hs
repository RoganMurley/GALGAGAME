module Profile.Apps where

import Config (App, runBeam)
import Data.Aeson ((.=), ToJSON(..), object)
import Database.Beam ((||.), (==.), all_, desc_, filter_, limit_, orderBy_, runSelectReturningList, runSelectReturningOne, select, val_)
import Data.Int (Int64)
import Data.Text (Text)
import Auth.Schema (PrimaryKey(UserId))
import Stats.Experience (Experience)
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Schema (Stats, StatsT(..))
import Replay.Schema (Replay, ReplayT(..))

loadProfile :: Text -> App (Maybe Profile)
loadProfile username = do
  statsResult <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> statsUser row ==. val_ (Auth.Schema.UserId username)) $
            all_ $ stats galgagameDb
  replaysResult <- loadProfileReplays username
  return $ hydrateReplays replaysResult . profileFromStats <$> statsResult

loadProfileReplays :: Text -> App [ProfileReplay]
loadProfileReplays username = do
  result <-
    runBeam $
      runSelectReturningList $
        select $
          filter_ (\row -> 
            (replayPlayerA row ==. val_ (Auth.Schema.UserId (Just username))) ||.
            (replayPlayerB row ==. val_ (Auth.Schema.UserId (Just username)))
          ) $
            limit_ 10  $
              orderBy_ (desc_ . Replay.Schema.replayCreated) $
                all_ $ replays galgagameDb
  return $ profileReplayFromReplay <$> result

-- Profile
data Profile = Profile
  { profile_name :: Text
  , profile_xp :: Experience
  , profile_replays :: [ProfileReplay]
  } deriving (Eq, Show)

instance ToJSON Profile where
  toJSON Profile { profile_name, profile_xp, profile_replays } =
    object
      [ "name" .= profile_name,
        "xp" .= profile_xp,
        "replays" .= profile_replays
      ]

profileFromStats :: Stats -> Profile
profileFromStats Stats {statsUser = (UserId username), statsExperience = xp} =
  Profile
    { profile_name = username
    , profile_xp = xp
    , profile_replays = []
    }

-- ProfileReplay
data ProfileReplay = ProfileReplay
  { profileReplay_pa :: Maybe Text
  , profileReplay_pb :: Maybe Text
  , profileReplay_id :: Int64
  } deriving (Eq, Show)

instance ToJSON ProfileReplay where
  toJSON ProfileReplay { profileReplay_pa, profileReplay_pb, profileReplay_id } =
    object
      [ "pa" .= profileReplay_pa,
        "pb" .= profileReplay_pb,
        "id" .= profileReplay_id
      ]

profileReplayFromReplay :: Replay -> ProfileReplay
profileReplayFromReplay Replay {replayPlayerA, replayPlayerB, replayId} =
  let
    fromPk (UserId username) = username
  in
    ProfileReplay
      { profileReplay_pa = fromPk replayPlayerA
      , profileReplay_pb = fromPk replayPlayerB
      , profileReplay_id = replayId
      }

hydrateReplays :: [ProfileReplay] -> Profile -> Profile
hydrateReplays replays profile =
  profile { profile_replays = replays }
