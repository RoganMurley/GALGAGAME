module Config where

import Control.Exception (catchJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.Pool (Pool, createPool, withResource)
import Database.Beam.Postgres (Pg, runBeamPostgres)

import qualified Database.Redis as Redis
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Database.PostgreSQL.Simple.Errors as Postgres


type App = ReaderT Config IO


data ConnectInfoConfig = ConnectInfoConfig
  { connectInfoConfig_redis    :: Redis.ConnectInfo
  , connectInfoConfig_postgres :: Postgres.ConnectInfo
  }


data Config = Config
  { redisConn  :: Redis.Connection
  , postgresPool :: Pool Postgres.Connection
  }


runApp :: ConnectInfoConfig -> App a -> IO a
runApp (ConnectInfoConfig redisInfo postgresInfo) app =
  do
    redisConn <- Redis.connect redisInfo
    postgresPool <- createPool (connectPostgres postgresInfo) Postgres.close 1 0.5 10
    let config = Config redisConn postgresPool
    runReaderT app config


connectPostgres :: Postgres.ConnectInfo -> IO Postgres.Connection
connectPostgres = Postgres.connectPostgreSQL . Postgres.postgreSQLConnectionString


runBeam :: Pg a -> App a
runBeam beam = do
  pool <- asks postgresPool
  withResource pool $ \conn -> liftIO $ runBeamPostgres conn beam


runBeamIntegrity :: Pg a -> App (Either Postgres.ConstraintViolation a)
runBeamIntegrity beam = do
  pool <- asks postgresPool
  withResource pool $ \conn ->
    liftIO $ catchJust Postgres.constraintViolation
      (runBeamPostgres conn beam >>= return . Right)
      (return . Left)


runRedis :: Redis.Redis a -> App a
runRedis redis = do
  conn <- asks redisConn
  liftIO $ Redis.runRedis conn redis


getTokenConn :: App Redis.Connection
getTokenConn = asks redisConn
