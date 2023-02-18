module Presence.Presence where

import qualified Auth.Schema as Auth
import Client (Client (..))
import qualified Client
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Control.Monad.STM (STM)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified User.User as User

newtype Presence = Presence (Map Text Client)
  deriving (Eq, Show)

new :: STM (TVar Presence)
new = newTVar $ Presence Map.empty

allClients :: Presence -> [Client]
allClients (Presence presence) = elems presence

instance ToJSON Presence where
  toJSON presence =
    let users :: [User.User]
        users = client_user <$> allClients presence
        authUsers :: [Auth.User]
        authUsers = catMaybes $ User.getAuthUser <$> users
        usernames :: [Text]
        usernames = Auth.userUsername <$> authUsers
     in object
          [ "online" .= usernames
          ]

addClient :: Client -> Presence -> Presence
addClient client (Presence presence) = Presence $ Map.insert (Client.guid client) client presence

removeClient :: Client -> Presence -> Presence
removeClient client (Presence presence) = Presence $ Map.delete (Client.guid client) presence
