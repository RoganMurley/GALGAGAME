module Feedback.Views where

import Config (ConnectInfoConfig, runApp)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object)
import Data.String.Conversions (cs)
import Network.HTTP.Types.Status (created201)
import Web.Scotty (ActionM, json, param, status)
import Web.Scotty.Cookie (getCookie)

import Auth.Apps (checkAuth, loginCookieName)
import Feedback.Apps (saveFeedback)


feedbackView :: ConnectInfoConfig -> ActionM ()
feedbackView config = do
  body <- param "body"
  token <- getCookie loginCookieName
  mUsername <- lift . runApp config $ checkAuth token
  liftIO $ runApp config $ saveFeedback (cs <$> mUsername) body
  json $ object []
  status created201
