module Config where

import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Database.Redis (Connection)


type App = ReaderT Config IO


data Config = Config
  { userConn   :: Connection
  , tokenConn  :: Connection
  , replayConn :: Connection
  }


runApp :: Config -> App a -> IO a
runApp = flip runReaderT


getConfig :: App Config
getConfig = ask


getUserConn :: App Connection
getUserConn = asks userConn


getTokenConn :: App Connection
getTokenConn = asks tokenConn


getReplayConn :: App Connection
getReplayConn = asks replayConn
