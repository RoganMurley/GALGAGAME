module User where

import qualified Auth.Apps as Auth
import qualified Auth.Schema as Auth
import Config (App, getApiKey, runBeam)
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Map as Map
import Data.Text (Text)
import Database.Beam (all_, filter_, runSelectReturningOne, select, val_, (==.))
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Stats (Experience)
import qualified Stats.Stats as Stats

data User = User Auth.User Experience | CpuUser Text | GuestUser | ServiceUser
  deriving (Show)

instance ToJSON User where
  toJSON user =
    object
      [ "name" .= getUsername user,
        "xp" .= getExperience user
      ]

getUsername :: User -> Text
getUsername (User user _) = Auth.userUsername user
getUsername (CpuUser name) = name
getUsername GuestUser = "guest"
getUsername ServiceUser = "service"

getQueryUsername :: User -> Maybe Text
getQueryUsername (User user xp) = Just . getUsername $ User user xp
getQueryUsername (CpuUser _) = Nothing
getQueryUsername GuestUser = Nothing
getQueryUsername ServiceUser = Nothing

getExperience :: User -> Experience
getExperience (User _ xp) = xp
getExperience _ = 0

getUserFromCookies :: Auth.Cookies -> App User
getUserFromCookies cookies = do
  let mLoginToken = Map.lookup Auth.sessionCookieName cookies
  mUsername <- Auth.checkAuth mLoginToken
  case mUsername of
    Nothing -> do
      let mApiToken = Map.lookup "api-key" cookies
      apiKey <- getApiKey
      if Just apiKey == mApiToken
        then return ServiceUser
        else return GuestUser
    Just username -> do
      mUser <-
        runBeam $
          runSelectReturningOne $
            select $
              filter_ (\row -> Auth.userUsername row ==. val_ username) $
                all_ $ users galgagameDb
      xp <- Stats.load username
      case mUser of
        Just user ->
          return $ User user xp
        Nothing ->
          return GuestUser

isSuperuser :: User -> Bool
isSuperuser ServiceUser = True
isSuperuser (User user _) = Auth.userSuperuser user
isSuperuser (CpuUser _) = False
isSuperuser GuestUser = False
