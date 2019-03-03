module Config where

import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import qualified Database.Redis as Redis
import qualified Database.PostgreSQL.Simple as Postgres


type App = ReaderT Config IO


data ConnectInfoConfig = ConnectInfoConfig
  { connectInfoConfig_token  :: Redis.ConnectInfo
  , connectInfoConfig_postgres :: Postgres.ConnectInfo
  }


data Config = Config
  { userConn   :: Postgres.Connection
  , tokenConn  :: Redis.Connection
  , replayConn :: Postgres.Connection
  }


runApp :: ConnectInfoConfig -> App a -> IO a
runApp (ConnectInfoConfig token replay) app =
  do
    tokenConn <- Redis.connect token
    postgresConn <- Postgres.connectPostgreSQL . Postgres.postgreSQLConnectionString $ replay
    let config = Config postgresConn tokenConn postgresConn
    runReaderT app config


getConfig :: App Config
getConfig = ask


getUserConn :: App Postgres.Connection
getUserConn = asks userConn


getTokenConn :: App Redis.Connection
getTokenConn = asks tokenConn


getReplayConn :: App Postgres.Connection
getReplayConn = asks replayConn
