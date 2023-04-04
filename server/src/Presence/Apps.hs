module Presence.Apps where

import Client (Client)
import Config (App, getPresence)
import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Data.Int (Int64)
import Presence.Presence (Presence, addClient, getClient, isUserIdOnline, removeClient)
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

isOnline :: Int64 -> App Bool
isOnline userId = isUserIdOnline userId <$> load

get :: Int64 -> App (Maybe Client)
get userId = getClient userId <$> load
