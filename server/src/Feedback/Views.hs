module Feedback.Views where

import Auth.Schema (UserT (..))
import Config (ConnectInfoConfig, runApp)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object)
import Feedback.Apps (saveFeedback)
import Network.HTTP.Types.Status (created201)
import qualified User
import Web.Scotty (ActionM, json, param, status)
import Web.Scotty.Cookie (getCookies)

feedbackView :: ConnectInfoConfig -> ActionM ()
feedbackView config = do
  body <- param "body"
  cookies <- getCookies
  user <- liftIO $ runApp config $ User.getUserFromCookies cookies ""
  let mUid =
        case user of
          User.User u _ ->
            Just $ userId u
          _ ->
            Nothing
  liftIO $ runApp config $ saveFeedback mUid body
  json $ object []
  status created201
