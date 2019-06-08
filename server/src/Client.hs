module Client where

import Config (App)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..))
import Data.Text (Text)
import Network.WebSockets (Connection, receiveData, sendTextData)
import User (User(..), getQueryUsername, getUsername)


data ClientConnection =
    PlayerConnection Connection
  | ComputerConnection


instance Show ClientConnection where
  show (PlayerConnection _) = "<Player Connection>"
  show ComputerConnection   = "<CPU Connection>"


data Client = Client
  { client_user       :: User
  , client_connection :: ClientConnection
  , client_guid       :: Text
  } deriving (Show)


instance Eq Client where
  Client{ client_guid = a } == Client{ client_guid = b } = a == b


instance ToJSON Client where
  toJSON client = toJSON . getUsername $ user client


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


send :: Text -> Client -> App ()
send message (Client _ (PlayerConnection conn) _) = liftIO $ sendTextData conn message
send _       _                                    = return ()


receive :: Client -> App (Text)
receive (Client _ (PlayerConnection conn) _) = liftIO $ receiveData conn
receive  _                                   = return ("")


cpuClient :: Text -> Client
cpuClient = Client CpuUser ComputerConnection
