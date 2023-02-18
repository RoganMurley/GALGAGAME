module Client where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (STM)
import Data.Text (Text)
import Network.WebSockets (Connection, receiveData, sendTextData)
import Stats.Progress (Progress (..))
import User.User (User (..), getProgress, getQueryUsername, getUsername)

data ClientConnection
  = PlayerConnection Connection
  | ComputerConnection

instance Show ClientConnection where
  show (PlayerConnection _) = "<Player Connection>"
  show ComputerConnection = "<CPU Connection>"

data Client = Client
  { client_user :: User,
    client_connection :: ClientConnection,
    client_guid :: Text
  }
  deriving (Show)

instance Eq Client where
  Client {client_guid = a} == Client {client_guid = b} = a == b

-- instance ToJSON Client where
--   toJSON Client {client_user} = toJSON client_user

user :: Client -> User
user = client_user

name :: Client -> Text
name = getUsername . client_user

connection :: Client -> ClientConnection
connection = client_connection

guid :: Client -> Text
guid = client_guid

queryUsername :: Client -> Maybe Text
queryUsername = getQueryUsername . client_user

send :: MonadIO m => Text -> Client -> m () -- App (), but more generic to avoid import cycle
send message (Client _ (PlayerConnection conn) _) = liftIO $ sendTextData conn message
send _ _ = return ()

receive :: MonadIO m => Client -> m Text -- App (), but more generic to avoid import cycle
receive (Client _ (PlayerConnection conn) _) = liftIO $ receiveData conn
receive _ = return ""

cpuClient :: Text -> Text -> Progress -> Client
cpuClient usernameIn guidIn progressIn = Client (CpuUser usernameIn progressIn) ComputerConnection guidIn

isCpu :: Client -> Bool
isCpu (Client _ ComputerConnection _) = True
isCpu _ = False

progress :: Client -> STM Progress
progress = getProgress . user
