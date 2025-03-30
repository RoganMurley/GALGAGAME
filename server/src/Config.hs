module Config where

import Control.Concurrent.Chan (Chan)
import Control.Concurrent.STM.TVar (TVar)
import Control.Exception (catchJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.Pool (Pool, createPool, withResource)
import Data.Text (Text)
import Database.Beam.Postgres (Pg, runBeamPostgres)
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.Errors qualified as Postgres
import Database.Redis qualified as Redis
import Network.Datadog qualified as DD
import Network.Datadog.Types qualified as DD
import Presence.Presence (Presence)
import System.Log.Logger (Priority)

type App = ReaderT Config IO

data ConnectInfoConfig = ConnectInfoConfig
  { connectInfoConfig_redis :: Redis.ConnectInfo,
    connectInfoConfig_postgres :: Postgres.ConnectInfo,
    connectInfoConfig_loggerChan :: Chan (Priority, String),
    connectInfoConfig_apiKey :: Text,
    connectInfoConfig_datadog :: (Maybe Text, Maybe Text),
    connectInfoConfig_presenceVar :: TVar Presence
  }

data Config = Config
  { redisConn :: Redis.Connection,
    postgresPool :: Pool Postgres.Connection,
    loggerChan :: Chan (Priority, String),
    datadogCreds :: Maybe DD.ReadWrite,
    apiKey :: Text,
    presenceVar :: TVar Presence
  }

runApp :: ConnectInfoConfig -> App a -> IO a
runApp config app =
  do
    let redisInfo = connectInfoConfig_redis config
    let postgresInfo = connectInfoConfig_postgres config
    let loggerChan = connectInfoConfig_loggerChan config
    let ddCredInput = connectInfoConfig_datadog config
    let apiKey = connectInfoConfig_apiKey config
    let presence = connectInfoConfig_presenceVar config

    redisConn <- Redis.connect redisInfo
    postgresPool <- createPool (connectPostgres postgresInfo) Postgres.close 1 0.5 10

    let ddCreds = case ddCredInput of
          (Just a, Just b) ->
            Just (a, b)
          _ ->
            Nothing

    runReaderT app $
      Config
        redisConn
        postgresPool
        loggerChan
        (uncurry DD.readWriteCredentials <$> ddCreds)
        apiKey
        presence

connectPostgres :: Postgres.ConnectInfo -> IO Postgres.Connection
connectPostgres = Postgres.connectPostgreSQL . Postgres.postgreSQLConnectionString

runBeam :: Pg a -> App a
runBeam beam = do
  pool <- asks postgresPool
  withResource pool $ \conn -> liftIO $ runBeamPostgres conn beam

runBeamTransaction :: Pg a -> App a
runBeamTransaction beam = do
  pool <- asks postgresPool
  withResource pool $
    \conn ->
      ( do
          liftIO $ Postgres.begin conn
          result <- liftIO $ runBeamPostgres conn beam
          liftIO $ Postgres.commit conn
          return result
      )

runBeamIntegrity :: Pg a -> App (Either Postgres.ConstraintViolation a)
runBeamIntegrity beam = do
  pool <- asks postgresPool
  withResource pool $ \conn ->
    liftIO $
      catchJust
        Postgres.constraintViolation
        ( do
            liftIO $ Postgres.begin conn
            result <- runBeamPostgres conn beam
            liftIO $ Postgres.commit conn
            return . Right $ result
        )
        (return . Left)

runRedis :: Redis.Redis a -> App a
runRedis redis = do
  conn <- asks redisConn
  liftIO $ Redis.runRedis conn redis

getTokenConn :: App Redis.Connection
getTokenConn = asks redisConn

getLoggerChan :: App (Chan (Priority, String))
getLoggerChan = asks loggerChan

getDatadogCreds :: App (Maybe DD.ReadWrite)
getDatadogCreds = asks datadogCreds

getApiKey :: App Text
getApiKey = asks apiKey

getPresence :: App (TVar Presence)
getPresence = asks presenceVar
