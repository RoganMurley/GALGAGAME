module User where

import Config (App, getApiKey, runBeam)
import Data.Text (Text)
import Database.Beam ((==.), all_, filter_, runSelectReturningOne, select, val_)
import Schema (GalgagameDb(..), galgagameDb)

import qualified Auth.Apps as Auth
import qualified Auth.Schema as Auth

import qualified Data.Map as Map


data User = User Auth.User | CpuUser Text | GuestUser | ServiceUser
  deriving (Show)


getUsername :: User -> Text
getUsername (User user)    = Auth.userUsername user
getUsername (CpuUser name) = name
getUsername GuestUser      = "guest"
getUsername ServiceUser    = "service"


getQueryUsername :: User -> Maybe Text
getQueryUsername (User user) = Just . getUsername $ User user
getQueryUsername (CpuUser _) = Nothing
getQueryUsername GuestUser   = Nothing
getQueryUsername ServiceUser = Nothing


getUserFromCookies :: Auth.Cookies -> App User
getUserFromCookies cookies = do
  let mLoginToken = Map.lookup Auth.sessionCookieName cookies
  mUsername <- Auth.checkAuth mLoginToken
  case mUsername of
    Nothing -> do
      let mApiToken = Map.lookup "api-key" cookies
      apiKey <- getApiKey
      if Just apiKey == mApiToken then
        return ServiceUser
          else
            return GuestUser
    Just username -> do
      mUser <- runBeam $ runSelectReturningOne $
        select $ filter_ (\row -> Auth.userUsername row ==. val_ username) $
          all_ $ users galgagameDb
      case mUser of
        Just user ->
          return $ User user
        Nothing ->
          return GuestUser


isSuperuser :: User -> Bool
isSuperuser ServiceUser = True
isSuperuser (User user) = Auth.userSuperuser user
isSuperuser (CpuUser _) = False
isSuperuser GuestUser   = False
