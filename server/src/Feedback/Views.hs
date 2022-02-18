module Feedback.Views where

import Auth.Apps (checkAuth)
import Config (ConnectInfoConfig, runApp)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object)
import Data.String.Conversions (cs)
import Feedback.Apps (saveFeedback)
import Network.HTTP.Types.Status (created201)
import Web.Scotty (ActionM, json, param, status)
import Web.Scotty.Cookie (getCookie)

feedbackView :: ConnectInfoConfig -> ActionM ()
feedbackView config = do
  body <- param "body"
  token <- getCookie "login"
  mUsername <- lift . runApp config $ checkAuth token
  liftIO $ runApp config $ saveFeedback (cs <$> mUsername) body
  json $ object []
  status created201
