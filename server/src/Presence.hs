module Presence where

import Client (Client (..))
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar)
import Control.Monad.STM (STM)
import Data.Map (Map)
import Data.Map as Map
import Data.Text (Text)

newtype Presence = Presence (Map Text Client)
  deriving (Eq, Show)

new :: STM (TVar Presence)
new = newTVar $ Presence Map.empty

allClients :: TVar Presence -> STM [Client]
allClients var = do
  Presence presence <- readTVar var
  return $ Map.elems presence
