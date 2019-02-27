module Config where

import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import qualified Database.Redis as Redis
import qualified Database.PostgreSQL.Simple as Postgres


type App = ReaderT Config IO


data ConnectInfoConfig = ConnectInfoConfig
  { connectInfoConfig_user   :: Redis.ConnectInfo
  , connectInfoConfig_token  :: Redis.ConnectInfo
  , connectInfoConfig_replay :: Postgres.ConnectInfo
  }


data Config = Config
  { userConn   :: Redis.Connection
  , tokenConn  :: Redis.Connection
  , replayConn :: Postgres.Connection
  }


runApp :: ConnectInfoConfig -> App a -> IO a
runApp (ConnectInfoConfig user token replay) app =
  do
    userConn <- Redis.connect user
    tokenConn <- Redis.connect token
    replayConn <- Postgres.connectPostgreSQL . Postgres.postgreSQLConnectionString $ replay
    let config = Config userConn tokenConn replayConn
    runReaderT app config


getConfig :: App Config
getConfig = ask


getUserConn :: App Redis.Connection
getUserConn = asks userConn


getTokenConn :: App Redis.Connection
getTokenConn = asks tokenConn


getReplayConn :: App Postgres.Connection
getReplayConn = asks replayConn
