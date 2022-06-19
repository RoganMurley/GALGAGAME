module User where

import qualified Auth.Apps as Auth
import qualified Auth.Schema as Auth
import Config (App, getApiKey, runBeam)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar)
import Control.Concurrent.STM.TVar (TVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (STM)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Traversable (forM)
import Database.Beam (all_, filter_, runSelectReturningOne, select, val_, (==.))
import Player (WhichPlayer (..))
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Progress (Progress (..), initialProgress)
import qualified Stats.Stats as Stats

data User
  = User Auth.User (TVar Progress)
  | CpuUser Text Progress
  | GuestUser Text (TVar Progress)
  | ServiceUser

instance Show User where
  show (User u _) = show u
  show (CpuUser _ _) = "CpuUser"
  show (GuestUser cid _) = "GuestUser" ++ show cid
  show ServiceUser = "ServiceUser"

instance Eq User where
  (==) (User u1 _) (User u2 _) = Auth.userUsername u1 == Auth.userUsername u2
  (==) (CpuUser _ _) (CpuUser _ _) = True
  (==) (GuestUser cid1 _) (GuestUser cid2 _) = cid1 == cid2
  (==) ServiceUser ServiceUser = True
  (==) _ _ = False

getUsername :: User -> Text
getUsername (User user _) = Auth.userUsername user
getUsername (CpuUser name _) = name
getUsername (GuestUser _ _) = "guest"
getUsername ServiceUser = "service"

getQueryUsername :: User -> Maybe Text
getQueryUsername (User user xp) = Just . getUsername $ User user xp
getQueryUsername (CpuUser _ _) = Nothing
getQueryUsername (GuestUser _ _) = Nothing
getQueryUsername ServiceUser = Nothing

getUserId :: User -> Maybe Int64
getUserId (User user _) = Just $ Auth.userId user
getUserId (CpuUser _ _) = Nothing
getUserId (GuestUser _ _) = Nothing
getUserId ServiceUser = Nothing

getProgress :: User -> STM Progress
getProgress (User _ progressVar) = readTVar progressVar
getProgress (CpuUser _ progress) = return progress
getProgress (GuestUser _ progressVar) = readTVar progressVar
getProgress _ = return initialProgress

setProgress :: Progress -> User -> STM ()
setProgress progress (User _ progressVar) = writeTVar progressVar progress
setProgress progress (GuestUser _ progressVar) = writeTVar progressVar progress
setProgress _ _ = return ()

getUserFromCookies :: Auth.Cookies -> Text -> App User
getUserFromCookies cookies cid = do
  let mLoginToken = Map.lookup Auth.sessionCookieName cookies
  mUsername <- Auth.checkAuth mLoginToken
  progressVar <- liftIO $ newTVarIO initialProgress
  case mUsername of
    Nothing -> do
      let mApiToken = Map.lookup "api-key" cookies
      apiKey <- getApiKey
      if Just apiKey == mApiToken
        then return ServiceUser
        else
          ( do
              let user = GuestUser cid progressVar
              progress <- Stats.load user
              liftIO . atomically $ writeTVar progressVar progress
              return user
          )
    Just username -> do
      mAuthUser <-
        runBeam $
          runSelectReturningOne $
            select $
              filter_ (\row -> Auth.userUsername row ==. val_ username) $
                all_ $ users galgagameDb
      case mAuthUser of
        Just authUser -> do
          let user = User authUser progressVar
          progress <- Stats.load user
          liftIO . atomically $ writeTVar progressVar progress
          return user
        Nothing ->
          return $ GuestUser cid progressVar

isSuperuser :: User -> Bool
isSuperuser ServiceUser = True
isSuperuser (User user _) = Auth.userSuperuser user
isSuperuser (CpuUser _ _) = False
isSuperuser (GuestUser _ _) = False

isHuman :: User -> Bool
isHuman ServiceUser = False
isHuman (User _ _) = True
isHuman (CpuUser _ _) = False
isHuman (GuestUser _ _) = True

isCpu :: User -> Bool
isCpu ServiceUser = False
isCpu (User _ _) = False
isCpu (CpuUser _ _) = True
isCpu (GuestUser _ _) = False

-- GameUser
data GameUser = GameUser
  { gameuser_progress :: Progress,
    gameuser_user :: User
  }
  deriving (Show)

instance ToJSON GameUser where
  toJSON GameUser {gameuser_user, gameuser_progress} =
    object
      [ "name" .= getUsername gameuser_user,
        "xp" .= progress_xp gameuser_progress
      ]

toGameUser :: User -> STM GameUser
toGameUser user = do
  progress <- getProgress user
  return $
    GameUser
      { gameuser_user = user,
        gameuser_progress = progress
      }

getUser :: WhichPlayer -> (Maybe GameUser, Maybe GameUser) -> Maybe GameUser
getUser PlayerA (ua, _) = ua
getUser PlayerB (_, ub) = ub

gameusersToUsers :: (Maybe GameUser, Maybe GameUser) -> (Maybe User, Maybe User)
gameusersToUsers (ua, ub) = (gameuser_user <$> ua, gameuser_user <$> ub)

usersToGameUsers :: (Maybe User, Maybe User) -> STM (Maybe GameUser, Maybe GameUser)
usersToGameUsers (mUserA, mUserB) = do
  a <- forM mUserA toGameUser
  b <- forM mUserB toGameUser
  return (a, b)
