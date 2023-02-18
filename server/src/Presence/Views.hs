module Presence.Views where

import Config (App, ConnectInfoConfig, getPresence, runApp)
import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Types.Status (ok200)
import Presence.Presence (Presence)
import Web.Scotty (ActionM, json, status)

view :: ConnectInfoConfig -> ActionM ()
view config = do
  presence <- lift $ runApp config loadPresence
  json presence
  status ok200

loadPresence :: App Presence
loadPresence = do
  presenceVar <- getPresence
  liftIO $ readTVarIO presenceVar
