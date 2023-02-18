module Presence.Apps where

import Client (Client)
import Config (App, getPresence)
import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Presence.Presence (Presence, addClient, removeClient)
import Util (modTVar)

load :: App Presence
load = do
  presenceVar <- getPresence
  liftIO $ readTVarIO presenceVar

add :: Client -> App ()
add client = do
  presenceVar <- getPresence
  liftIO . atomically $ modTVar presenceVar (addClient client)

remove :: Client -> App ()
remove client = do
  presenceVar <- getPresence
  liftIO . atomically $ modTVar presenceVar (removeClient client)
