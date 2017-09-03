module Client where

import Data.Aeson (ToJSON(..))
import Data.Text (Text)
import Network.WebSockets (Connection, receiveData, sendTextData)

import Username (Username(Username))


data ClientConnection =
    PlayerConnection Connection
  | ComputerConnection


instance Show ClientConnection where
  show (PlayerConnection _) = "<Player Connection>"
  show ComputerConnection   = "<CPU Connection>"


data Client = Client
  { client_name       :: Username
  , client_connection :: ClientConnection
  , client_guid       :: Text
  } deriving (Show)


instance Eq Client where
  Client{ client_guid = a } == Client{ client_guid = b } = a == b


instance ToJSON Client where
  toJSON Client{ client_name } = toJSON client_name


name :: Client -> Username
name = client_name


connection :: Client -> ClientConnection
connection = client_connection


guid :: Client -> Text
guid = client_guid


send :: Text -> Client -> IO ()
send message (Client _ (PlayerConnection conn) _) = sendTextData conn message
send _       _                                    = return ()


receive :: Client -> IO (Text)
receive (Client _ (PlayerConnection conn) _) = receiveData conn
receive  _                                   = return ("")


cpuClient :: Text -> Client
cpuClient = Client (Username "CPU") ComputerConnection
