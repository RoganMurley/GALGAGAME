module League.Views where

import Auth.Apps (checkAuth)
import Config (ConnectInfoConfig, runApp)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object)
import Data.String.Conversions (cs)
import League.Apps (checkLeague, saveLeague)
import Network.HTTP.Types.Status (created201, notFound404, ok200, unauthorized401)
import Web.Scotty (ActionM, json, status)
import Web.Scotty.Cookie (getCookie)

leagueView :: ConnectInfoConfig -> ActionM ()
leagueView config = do
  token <- getCookie "login"
  mUsername <- lift . runApp config $ checkAuth token
  case mUsername of
    Nothing -> status unauthorized401
    Just username -> do
      liftIO $ runApp config $ saveLeague (cs username)
      json $ object []
      status created201

leagueCheckView :: ConnectInfoConfig -> ActionM ()
leagueCheckView config = do
  token <- getCookie "login"
  mUsername <- lift . runApp config $ checkAuth token
  case mUsername of
    Nothing -> status unauthorized401
    Just username -> do
      exists <- liftIO $ runApp config $ checkLeague (cs username)
      if exists
        then
          ( do
              json $ object []
              status ok200
          )
        else status notFound404
