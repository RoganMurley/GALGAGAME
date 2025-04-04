module Feedback.Views where

import Auth.Schema (UserT (..))
import Config (ConnectInfoConfig, runApp)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object)
import Data.Map qualified as Map
import Feedback.Apps (saveFeedback)
import Network.HTTP.Types.Status (created201)
import User.Apps qualified as User
import User.User qualified as User
import Web.Scotty (ActionM, formParam, json, status)
import Web.Scotty.Cookie (getCookies)

feedbackView :: ConnectInfoConfig -> ActionM ()
feedbackView config = do
  body <- formParam "body"
  cookies <- Map.fromList <$> getCookies
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
