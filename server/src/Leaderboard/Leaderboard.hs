module Leaderboard.Leaderboard where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Text (Text)
import Auth.Schema (PrimaryKey(UserId))
import Stats.Experience (Experience)
import Stats.Schema (Stats, StatsT(..))

newtype Leaderboard = Leaderboard [Entry]
  deriving (Eq, Show)

instance ToJSON Leaderboard where
  toJSON (Leaderboard entries) =
    object [ "leaderboard" .= entries ]

data Entry = Entry
  { entry_name :: Text
  , entry_xp :: Experience
  } deriving (Eq, Show)

instance ToJSON Entry where
  toJSON Entry { entry_name, entry_xp} =
    object
      [ "name" .= entry_name,
        "xp" .= entry_xp
      ]

entryFromStats :: Stats -> Entry
entryFromStats Stats {statsUser = (UserId username), statsExperience = xp} =
  Entry { entry_name = username, entry_xp = xp }
