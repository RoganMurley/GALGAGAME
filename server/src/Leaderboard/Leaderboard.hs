module Leaderboard.Leaderboard where

import Auth.Schema (User, UserT (..))
import Data.Aeson (ToJSON (..), object, (.=))
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

entryFromStats :: User.User -> (Stats, Maybe User) -> Entry
entryFromStats gameuser (Stats {statsExperience = xp}, mUser) =
  case mUser of
    Just User {userId, userUsername} ->
      Entry
        { entry_name = userUsername,
          entry_xp = xp,
          entry_is_me = Just userId == User.getUserId gameuser
        }
    Nothing ->
      Entry {entry_name = "???", entry_xp = xp, entry_is_me = False}
