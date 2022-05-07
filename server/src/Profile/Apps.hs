module Profile.Apps where

import Auth.Schema (PrimaryKey (UserId), UserId, UserT (..))
import Config (App, runBeam)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (LocalTime)
import Database.Beam (all_, desc_, filter_, leftJoin_, limit_, orderBy_, references_, runSelectReturningList, runSelectReturningOne, select, val_, (==.), (||.))
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
      return $
        Just $
          hydrateReplays replaysResult $
            profileFromStats (userUsername user) (statsExperience stats)
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
                replayPlayerB r
              )
          )
  let getCreated = \(_, x, _, _) -> x
  let getPlayerA = \(_, _, x, _) -> x
  let getPlayerB = \(_, _, _, x) -> x
  result <-
    runBeam $
      runSelectReturningList $
        select $
          limit_ 10 $
            orderBy_ (desc_ . getCreated) $
              filter_
                ( \row ->
                    (getPlayerA row ==. val_ (Auth.Schema.UserId (Just userId)))
                      ||. (getPlayerB row ==. val_ (Auth.Schema.UserId (Just userId)))
                )
                $ columnSubset $ all_ $ replays galgagameDb
  return $ profileReplayFromReplay <$> result

-- Profile
data Profile = Profile
  { profile_name :: Text,
    profile_xp :: Experience,
    profile_replays :: [ProfileReplay]
  }
  deriving (Eq, Show)

instance ToJSON Profile where
  toJSON Profile {profile_name, profile_xp, profile_replays} =
    object
      [ "name" .= profile_name,
        "xp" .= profile_xp,
        "replays" .= profile_replays
      ]

profileFromStats :: Text -> Experience -> Profile
profileFromStats username xp =
  Profile
    { profile_name = username,
      profile_xp = xp,
      profile_replays = []
    }

-- ProfileReplay
data ProfileReplay = ProfileReplay
  { profileReplay_pa :: Maybe Int64,
    profileReplay_pb :: Maybe Int64,
    profileReplay_id :: Int64
  }
  deriving (Eq, Show)

instance ToJSON ProfileReplay where
  toJSON ProfileReplay {profileReplay_pa, profileReplay_pb, profileReplay_id} =
    object
      [ "pa" .= profileReplay_pa,
        "pb" .= profileReplay_pb,
        "id" .= profileReplay_id
      ]

profileReplayFromReplay :: (Int64, LocalTime, Maybe UserId, Maybe UserId) -> ProfileReplay
profileReplayFromReplay (replayId, _, replayPlayerA, replayPlayerB) =
  let fromPk :: UserId -> Int64
      fromPk (UserId uid) = uid
   in ProfileReplay
        { profileReplay_pa = fromPk <$> replayPlayerA,
          profileReplay_pb = fromPk <$> replayPlayerB,
          profileReplay_id = replayId
        }

hydrateReplays :: [ProfileReplay] -> Profile -> Profile
hydrateReplays replays profile =
  profile {profile_replays = replays}
