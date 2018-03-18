module Database where

import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Safe (readMay)

import qualified Database.Redis as R


data Database =
    UserDatabase
  | TokenDatabase
  | ReplayDatabase


connectInfo :: (Maybe String, Maybe String, Maybe String) -> Database -> R.ConnectInfo
connectInfo (host, portString, password) database =
  R.defaultConnectInfo
    { R.connectAuth     = cs <$> password
    , R.connectHost     = fromMaybe defaultHost host
    , R.connectPort     = fromMaybe defaultPort port
    , R.connectDatabase = asId database
    }
  where
    asId :: Database -> Integer
    asId UserDatabase  = 0
    asId TokenDatabase = 1
    asId ReplayDatabase = 2
    defaultHost :: R.HostName
    defaultHost = "redis"
    defaultPort :: R.PortID
    defaultPort = R.PortNumber 6379
    port :: Maybe R.PortID
    port = R.PortNumber <$> (portString >>= readMay)
