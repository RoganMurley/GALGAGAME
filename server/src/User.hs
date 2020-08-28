module User where

import Config (App, runBeam)
import Data.Text (Text)
import Database.Beam ((==.), all_, filter_, runSelectReturningOne, select, val_)
import Schema (GalgagameDb(..), galgagameDb)

import qualified Auth.Apps as Auth
import qualified Auth.Schema as Auth


data User = User Auth.User | CpuUser | GuestUser
  deriving (Show)


getUsername :: User -> Text
getUsername (User user) = Auth.userUsername user
getUsername CpuUser     = "cpu"
getUsername GuestUser   = "guest"


getQueryUsername :: User -> Maybe Text
getQueryUsername (User user) = Just . getUsername $ User user
getQueryUsername CpuUser     = Nothing
getQueryUsername GuestUser   = Nothing


getUserFromToken :: Maybe Auth.Token -> App User
getUserFromToken mToken = do
  mUsername <- Auth.checkAuth mToken
  case mUsername of
    Nothing ->
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
