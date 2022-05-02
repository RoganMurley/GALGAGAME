module Profile.Apps where

import Config (App, runBeam)
import Data.Aeson ((.=), ToJSON(..), object)
import Database.Beam ((==.), all_, filter_, runSelectReturningOne, select, val_)
import Data.Text (Text)
import Auth.Schema (PrimaryKey(UserId))
import Stats.Experience (Experience)
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Schema (Stats, StatsT(..))

loadProfile :: Text -> App (Maybe Profile)
loadProfile username = do
  result <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> statsUser row ==. val_ (Auth.Schema.UserId username)) $
            all_ $ stats galgagameDb
  return $ profileFromStats <$> result

data Profile = Profile
  { profile_name :: Text
  , profile_xp :: Experience
  } deriving (Eq, Show)

instance ToJSON Profile where
  toJSON Profile { profile_name, profile_xp} =
    object
      [ "name" .= profile_name,
        "xp" .= profile_xp
      ]

profileFromStats :: Stats -> Profile
profileFromStats Stats {statsUser = (UserId username), statsExperience = xp} =
  Profile { profile_name = username, profile_xp = xp }
