module Profile.Views where

import Config (ConnectInfoConfig, runApp)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object)
import Network.HTTP.Types.Status (notFound404, ok200)
import Profile.Apps (loadProfile)
import Web.Scotty (ActionM, json, pathParam, status)

profileView :: ConnectInfoConfig -> ActionM ()
profileView config = do
  username <- pathParam "username"
  mProfile <- lift . runApp config $ loadProfile username
  case mProfile of
    Just profile -> do
      json profile
      status ok200
    Nothing -> do
      json $ object []
      status notFound404
