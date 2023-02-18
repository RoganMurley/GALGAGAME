module Presence.Presence where

import qualified Auth.Schema as Auth
import Client (Client (..))
import qualified Client
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Control.Monad.STM (STM)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Int (Int64)
import Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified User.User as User

newtype Presence = Presence (Map Int64 (Client, Int))
  deriving (Eq, Show)

new :: STM (TVar Presence)
new = newTVar $ Presence Map.empty

allClients :: Presence -> [Client]
allClients (Presence presence) = fst <$> elems presence

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
addClient client (Presence presence) =
  let mUserId = User.getUserId $ Client.user client
   in case mUserId of
        Just userId ->
          case Map.lookup userId presence of
            Just (_, n) ->
              Presence $ Map.insert userId (client, n + 1) presence
            Nothing ->
              Presence $ Map.insert userId (client, 1) presence
        Nothing ->
          Presence presence

removeClient :: Client -> Presence -> Presence
removeClient client (Presence presence) =
  let mUserId = User.getUserId $ Client.user client
   in case mUserId of
        Just userId ->
          case Map.lookup userId presence of
            Just (_, n) ->
              if n <= 1
                then Presence $ Map.delete userId presence
                else Presence $ Map.insert userId (client, n -1) presence
            Nothing ->
              Presence presence
        Nothing ->
          Presence presence

isUserIdOnline :: Int64 -> Presence -> Bool
isUserIdOnline userId (Presence presence) = Map.member userId presence
