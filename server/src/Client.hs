module Client where

import Data.Text (Text)
import Network.WebSockets (Connection, sendTextData)

import GameState (Username)


data ClientConnection =
    PlayerConnection Connection
  | ComputerConnection


instance Show ClientConnection where
  show (PlayerConnection _) = "<Player Connection>"
  show ComputerConnection   = "<CPU Connection>"


data Client = Client
  { client_name       :: Username
  , client_connection :: ClientConnection
  } deriving (Show)


name :: Client -> Username
name = client_name


connection :: Client -> ClientConnection
connection = client_connection


send :: Text -> Client -> IO ()
send message (Client _ (PlayerConnection conn)) = sendTextData conn message
send _       _                                  = return ()


cpuClient :: Client
cpuClient = Client "CPU" ComputerConnection
