module User.Apps where

import qualified Auth.Apps as Auth
import qualified Auth.Schema as Auth
import Config (App, getApiKey, runBeam)
import Control.Concurrent.STM.TVar (newTVarIO, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import qualified Data.Map as Map
import Data.Text (Text)
import Database.Beam (all_, filter_, runSelectReturningOne, select, val_, (==.))
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Progress (initialProgress)
import qualified Stats.Stats as Stats
import User.User (User (..))

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