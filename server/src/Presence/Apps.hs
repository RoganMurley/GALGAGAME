module Presence.Apps where

import Client (Client)
import Config (App, getPresence)
import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Data.Int (Int64)
import qualified Log
import Presence.Presence (Presence, addClient, isUserIdOnline, removeClient)
import Util (modTVar)

load :: App Presence
load = do
  presenceVar <- getPresence
  presence <- liftIO $ readTVarIO presenceVar
  Log.debug $ show presence
  return presence

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
