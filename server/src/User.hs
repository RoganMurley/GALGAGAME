module User where

import Config (App, runBeam)
import Data.Text (Text)
import Database.Beam ((==.), all_, filter_, runSelectReturningOne, select, val_)
import Schema (RingOfWorldsDb(..), ringOfWorldsDb)

import qualified Auth.Apps as Auth
import qualified Auth.Schema as Auth


data User = User Auth.User | CpuUser | GuestUser
  deriving (Show)


getUsername :: User -> Text
getUsername (User user) = Auth.userUsername user
getUsername CpuUser     = "cpu"
getUsername GuestUser   = "guest"


getUserFromToken :: Maybe Auth.Token -> App User
getUserFromToken mToken = do
  mUsername <- Auth.checkAuth mToken
  case mUsername of
    Nothing ->
      return GuestUser
    Just username -> do
      mUser <- runBeam $ runSelectReturningOne $
        select $ filter_ (\row -> Auth.userUsername row ==. val_ username) $
          all_ $ users ringOfWorldsDb
      case mUser of
        Just user ->
          return $ User user
        Nothing ->
          return GuestUser
