module Profile.Apps where

import Auth.Schema (PrimaryKey (UserId), UserId, UserT (..))
import Config (App, runBeam)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (LocalTime)
import Database.Beam (all_, desc_, filter_, leftJoin_, limit_, orderBy_, references_, runSelectReturningList, runSelectReturningOne, select, val_, (==.), (||.))
import Presence.Apps qualified as Presence
import Replay.Schema (ReplayT (..))
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Experience (Experience)
import Stats.Schema (StatsT (..))

loadProfile :: Text -> App (Maybe Profile)
loadProfile username = do
  mUserStats <-
    runBeam $
      runSelectReturningOne $
        select $ do
          user <- filter_ (\row -> userUsername row ==. val_ username) $ all_ $ users galgagameDb
          stats <- leftJoin_ (all_ (stats galgagameDb)) (\stats -> statsUser stats `references_` user)
          pure (user, stats)
  case mUserStats of
    Just (user, Just stats) -> do
      replaysResult <- loadProfileReplays (userId user)
      isOnline <- Presence.isOnline (userId user)
      return $
        Just $
          hydrateOnline isOnline $
            hydrateReplays replaysResult $
              profileFromStats (userUsername user) (userId user) (statsExperience stats)
    _ ->
      return Nothing

loadProfileReplays :: Int64 -> App [ProfileReplay]
loadProfileReplays userId = do
  let columnSubset =
        fmap
          ( \r ->
              ( replayId r,
                replayCreated r,
                replayPlayerA r,
                replayPlayerB r,
                replayDisplayUsernameA r,
                replayDisplayUsernameB r
              )
          )
  let getCreated = \(_, x, _, _, _, _) -> x
  let getPlayerA = \(_, _, x, _, _, _) -> x
  let getPlayerB = \(_, _, _, x, _, _) -> x
  result <-
    runBeam
      $ runSelectReturningList
      $ select
      $ limit_ 10
      $ orderBy_ (desc_ . getCreated)
      $ filter_
        ( \row ->
            (getPlayerA row ==. val_ (Auth.Schema.UserId (Just userId)))
              ||. (getPlayerB row ==. val_ (Auth.Schema.UserId (Just userId)))
        )
      $ columnSubset
      $ all_
      $ replays galgagameDb
  return $ profileReplayFromReplay <$> result

-- Profile
data Profile = Profile
  { profile_name :: Text,
    profile_id :: Int64,
    profile_xp :: Experience,
    profile_replays :: [ProfileReplay],
    profile_online :: Bool
  }
  deriving (Eq, Show)

instance ToJSON Profile where
  toJSON Profile {profile_name, profile_id, profile_xp, profile_replays, profile_online} =
    object
      [ "name" .= profile_name,
        "id" .= profile_id,
        "xp" .= profile_xp,
        "replays" .= profile_replays,
        "online" .= profile_online
      ]

profileFromStats :: Text -> Int64 -> Experience -> Profile
profileFromStats username uid xp =
  Profile
    { profile_name = username,
      profile_id = uid,
      profile_xp = xp,
      profile_replays = [],
      profile_online = False
    }

-- ProfileReplay
data ProfileReplay = ProfileReplay
  { profileReplay_pa :: Maybe Text,
    profileReplay_pb :: Maybe Text,
    profileReplay_id :: Int64
  }
  deriving (Eq, Show)

instance ToJSON ProfileReplay where
  toJSON
    ProfileReplay
      { profileReplay_pa,
        profileReplay_pb,
        profileReplay_id
      } =
      object
        [ "pa" .= profileReplay_pa,
          "pb" .= profileReplay_pb,
          "id" .= profileReplay_id
        ]

profileReplayFromReplay :: (Int64, LocalTime, Maybe UserId, Maybe UserId, Maybe Text, Maybe Text) -> ProfileReplay
profileReplayFromReplay (replayId, _, _, _, displayUsernamePa, displayUsernamePb) =
  ProfileReplay
    { profileReplay_id = replayId,
      profileReplay_pa = displayUsernamePa,
      profileReplay_pb = displayUsernamePb
    }

hydrateReplays :: [ProfileReplay] -> Profile -> Profile
hydrateReplays replays profile =
  profile {profile_replays = replays}

hydrateOnline :: Bool -> Profile -> Profile
hydrateOnline isOnline profile = profile {profile_online = isOnline}
