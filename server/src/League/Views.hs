module League.Views where

import Auth.Schema (UserT (..))
import Config (ConnectInfoConfig, runApp)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object)
import League.Apps (checkLeague, saveLeague)
import Network.HTTP.Types.Status (created201, notFound404, ok200, unauthorized401)
import User.Apps qualified as User
import User.User qualified as User
import Web.Scotty (ActionM, json, status)
import Web.Scotty.Cookie (getCookies)

leagueView :: ConnectInfoConfig -> ActionM ()
leagueView config = do
  cookies <- getCookies
  user <- liftIO $ runApp config $ User.getUserFromCookies cookies ""
  case user of
    User.User u _ -> do
      liftIO $ runApp config $ saveLeague (userId u)
      json $ object []
      status created201
    _ ->
      status unauthorized401

leagueCheckView :: ConnectInfoConfig -> ActionM ()
leagueCheckView config = do
  cookies <- getCookies
  user <- liftIO $ runApp config $ User.getUserFromCookies cookies ""
  case user of
    User.User u _ -> do
      exists <- liftIO $ runApp config $ checkLeague (userId u)
      if exists
        then
          ( do
              json $ object []
              status ok200
          )
        else status notFound404
    _ ->
      status unauthorized401