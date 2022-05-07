module Leaderboard.Leaderboard where

import Auth.Schema (User, UserT (..))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Stats.Experience (Experience)
import Stats.Schema (Stats, StatsT (..))

newtype Leaderboard = Leaderboard [Entry]
  deriving (Eq, Show)

instance ToJSON Leaderboard where
  toJSON (Leaderboard entries) =
    object ["leaderboard" .= entries]

data Entry = Entry
  { entry_name :: Text,
    entry_xp :: Experience
  }
  deriving (Eq, Show)

instance ToJSON Entry where
  toJSON Entry {entry_name, entry_xp} =
    object
      [ "name" .= entry_name,
        "xp" .= entry_xp
      ]

entryFromStats :: (Stats, Maybe User) -> Entry
entryFromStats (Stats {statsExperience = xp}, mUser) =
  case mUser of
    Just User {userUsername = username} ->
      Entry {entry_name = username, entry_xp = xp}
    Nothing ->
      Entry {entry_name = "???", entry_xp = xp}
