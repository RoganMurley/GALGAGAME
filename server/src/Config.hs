module Config where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Database.Beam.Postgres (Pg, runBeamPostgres)

import qualified Database.Redis as Redis
import qualified Database.PostgreSQL.Simple as Postgres


type App = ReaderT Config IO


data ConnectInfoConfig = ConnectInfoConfig
  { connectInfoConfig_redis    :: Redis.ConnectInfo
  , connectInfoConfig_postgres :: Postgres.ConnectInfo
  }


data Config = Config
  { redisConn  :: Redis.Connection
  , postgresConn :: Postgres.Connection
  }


runApp :: ConnectInfoConfig -> App a -> IO a
runApp (ConnectInfoConfig redisInfo postgresInfo) app =
  do
    redisConn <- Redis.connect redisInfo
    postgresConn <- Postgres.connectPostgreSQL . Postgres.postgreSQLConnectionString $ postgresInfo
    let config = Config redisConn postgresConn
    runReaderT app config


runBeam :: Pg a -> App a
runBeam beam = do
  conn <- asks postgresConn
  liftIO $ runBeamPostgres conn $ beam


runRedis :: Redis.Redis a -> App a
runRedis redis = do
  conn <- asks redisConn
  liftIO $ Redis.runRedis conn redis


getTokenConn :: App Redis.Connection
getTokenConn = asks redisConn
