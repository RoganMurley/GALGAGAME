module Presence.Presence where

import qualified Auth.Schema as Auth
import Client (Client (..))
import qualified Client
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Control.Monad.STM (STM)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Bifunctor (second)
import Data.Int (Int64)
import Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified User.User as User

newtype Presence = Presence (Map Int64 (Client, Int))
  deriving (Eq, Show)

new :: STM (TVar Presence)
new = newTVar $ Presence Map.empty

allClients :: Presence -> [(Int64, Client)]
allClients (Presence presence) = (\(a, (b, _)) -> (a, b)) <$> assocs presence

instance ToJSON Presence where
  toJSON presence =
    let users :: [(Int64, User.User)]
        users = second client_user <$> allClients presence
        authUsers :: [(Int64, Auth.User)]
        authUsers =
          catMaybes $
            ( \(uid, user) -> case User.getAuthUser user of
                Just authUser ->
                  Just (uid, authUser)
                Nothing ->
                  Nothing
            )
              <$> users
        usernames :: [(Int64, Text)]
        usernames = second Auth.userUsername <$> authUsers
     in object
          [ "online"
              .= fmap
                (\(uid, username) -> object ["id" .= uid, "username" .= username])
                usernames
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

getClient :: Int64 -> Presence -> Maybe Client
getClient userId (Presence presence) = fst <$> Map.lookup userId presence

isUserIdOnline :: Int64 -> Presence -> Bool
isUserIdOnline userId (Presence presence) = Map.member userId presence
