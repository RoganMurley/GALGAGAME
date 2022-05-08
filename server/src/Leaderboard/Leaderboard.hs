module Leaderboard.Leaderboard where

import Auth.Schema (User, UserT (..))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import Stats.Experience (Experience)
import Stats.Schema (Stats, StatsT (..))
import qualified User

newtype Leaderboard = Leaderboard [Entry]
  deriving (Eq, Show)

instance ToJSON Leaderboard where
  toJSON (Leaderboard entries) =
    object ["leaderboard" .= entries]

data Entry = Entry
  { entry_name :: Text,
    entry_xp :: Experience,
    entry_user_id :: Int64,
    entry_is_me :: Bool
  }
  deriving (Eq, Show)

instance ToJSON Entry where
  toJSON Entry {entry_name, entry_xp, entry_is_me} =
    object
      [ "name" .= entry_name,
        "xp" .= entry_xp,
        "is_me" .= entry_is_me
      ]

entryFromStats :: (Stats, Maybe User) -> Maybe Entry
entryFromStats (Stats {statsExperience = xp}, mUser) =
  case mUser of
    Just User {userId, userUsername} ->
      Just $
        Entry
          { entry_name = userUsername,
            entry_xp = xp,
            entry_user_id = userId,
            entry_is_me = False
          }
    Nothing ->
      Nothing

hydrateIsMe :: User.User -> Leaderboard -> Leaderboard
hydrateIsMe user (Leaderboard entries) =
  let hydrateEntry :: User.User -> Entry -> Entry
      hydrateEntry u entry =
        entry
          { entry_is_me =
              User.getUserId u == Just (entry_user_id entry)
          }
   in Leaderboard $ hydrateEntry user <$> entries
