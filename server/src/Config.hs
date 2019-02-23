module Config where

import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import qualified Database.Redis as Redis
import qualified Database.PostgreSQL.Simple as Postgres


type App = ReaderT Config IO


data Config = Config
  { userConn   :: Redis.Connection
  , tokenConn  :: Redis.Connection
  , replayConn :: Postgres.Connection
  }


runApp :: Config -> App a -> IO a
runApp = flip runReaderT


getConfig :: App Config
getConfig = ask


getUserConn :: App Redis.Connection
getUserConn = asks userConn


getTokenConn :: App Redis.Connection
getTokenConn = asks tokenConn


getReplayConn :: App Postgres.Connection
getReplayConn = asks replayConn
