module Database where

import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Safe (readMay)
import Data.Word (Word16)

import qualified Database.Redis as Redis
import qualified Database.PostgreSQL.Simple as Postgres


data RedisDatabase =
    UserDatabase
  | TokenDatabase


redisConnectInfo :: (Maybe String, Maybe String, Maybe String) -> RedisDatabase -> Redis.ConnectInfo
redisConnectInfo (host, portString, password) database =
  Redis.defaultConnectInfo
    { Redis.connectAuth     = cs <$> password
    , Redis.connectHost     = fromMaybe defaultHost host
    , Redis.connectPort     = fromMaybe defaultPort port
    , Redis.connectDatabase = asId database
    }
  where
    asId :: RedisDatabase -> Integer
    asId UserDatabase  = 0
    asId TokenDatabase = 1
    defaultHost :: Redis.HostName
    defaultHost = "redis"
    defaultPort :: Redis.PortID
    defaultPort = Redis.PortNumber 6379
    port :: Maybe Redis.PortID
    port = Redis.PortNumber <$> (portString >>= readMay)


postgresConnectInfo :: (Maybe String, Maybe String, Maybe String, Maybe String, Maybe String) -> Postgres.ConnectInfo
postgresConnectInfo (host, portString, user, password, database) =
  Postgres.defaultConnectInfo
    { Postgres.connectHost     = fromMaybe defaultHost host
    , Postgres.connectPort     = fromMaybe defaultPort port
    , Postgres.connectUser     = fromMaybe defaultUser user
    , Postgres.connectPassword = fromMaybe defaultPassword password
    , Postgres.connectDatabase = fromMaybe defaultDatabase database
    }
  where
    defaultHost :: String
    defaultHost = "database"
    defaultPort :: Word16
    defaultPort = 5432
    defaultUser :: String
    defaultUser = "postgres"
    defaultPassword :: String
    defaultPassword = "example"
    defaultDatabase :: String
    defaultDatabase = "postgres"
    port :: Maybe Word16
    port = portString >>= readMay
