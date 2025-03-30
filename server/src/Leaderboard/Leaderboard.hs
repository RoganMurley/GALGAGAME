module Leaderboard.Leaderboard where

import Auth.Schema (PrimaryKey (UserId), User, UserId, UserT (..))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Int (Int32, Int64)
import Data.List (sortBy)
import Data.Text (Text)
import Stats.Experience (Experience)
import Stats.Schema (Stats, StatsT (..))
import User.User qualified as User

newtype Leaderboard = Leaderboard [Entry]
  deriving (Eq, Show)

instance ToJSON Leaderboard where
  toJSON (Leaderboard entries) =
    object ["leaderboard" .= entries]

data Entry = Entry
  { entry_name :: Text,
    entry_xp :: Experience,
    entry_user_id :: Int64,
    entry_rank :: Int32,
    entry_is_me :: Bool
  }
  deriving (Eq, Show)

instance ToJSON Entry where
  toJSON Entry {entry_name, entry_xp, entry_rank, entry_is_me} =
    object
      [ "name" .= entry_name,
        "xp" .= entry_xp,
        "rank" .= entry_rank,
        "is_me" .= entry_is_me
      ]

entryFromDb :: ((Stats, Maybe User), Int32) -> Maybe Entry
entryFromDb ((Stats {statsExperience = xp}, mUser), rank) =
  case mUser of
    Just User {userId, userUsername} ->
      Just $
        Entry
          { entry_name = userUsername,
            entry_xp = xp,
            entry_user_id = userId,
            entry_rank = rank,
            entry_is_me = False
          }
    Nothing ->
      Nothing

entryFromStats :: User.User -> (Stats, Int32) -> Entry
entryFromStats user (Stats {statsExperience = xp, statsUser = userId}, rank) =
  let fromPk :: UserId -> Int64
      fromPk (UserId uid) = uid
   in Entry
        { entry_name = User.getUsername user,
          entry_xp = xp,
          entry_user_id = fromPk userId,
          entry_rank = rank,
          entry_is_me = Just (fromPk userId) == User.getUserId user
        }

hydrateWithMe :: Entry -> Leaderboard -> Leaderboard
hydrateWithMe entry (Leaderboard entries) =
  Leaderboard $
    sortBy (\a b -> compare (entry_rank a) (entry_rank b)) $
      (++) [entry] $
        filter
          (\e -> entry_user_id e /= entry_user_id entry)
          entries
