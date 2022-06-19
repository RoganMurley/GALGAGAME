module Main where

import Act (actOutcome, actPlay, actSpec, syncClient, syncRoomMetadata)
import ArtificialIntelligence (Action (..), chooseAction)
import qualified Auth.Apps as Auth
import qualified Auth.Views as Auth
import Client (Client (..), ClientConnection (..))
import qualified Client
import Command (Command (..))
import qualified Command
import Config (App, ConnectInfoConfig (..), runApp)
import Control.Concurrent.Chan (newChan)
import Control.Concurrent.Lifted (fork, killThread, threadDelay)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO)
import Control.Exception.Lifted (finally)
import Control.Monad (forM_, forever, mzero, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (STM, atomically)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.State (StateT, get, modify', put, runStateT)
import qualified DSL.Beta as Beta
import qualified Data.GUID as GUID
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime)
import Database (postgresConnectInfo, redisConnectInfo)
import DeckBuilding (ChosenCharacter (..), Rune (..), UnchosenCharacter (..))
import qualified DeckBuilding
import Encounter (updateRoomEncounter)
import GHC.Conc (setUncaughtExceptionHandler)
import GameState (GameState (..), PlayState (..), PlayingR (..), WaitType (..), isWinner)
import qualified Log
import qualified Metrics
import Model (Turn)
import Negotiation (Prefix (..), Request (..))
import qualified Negotiation
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import Outcome (Outcome)
import Player (WhichPlayer (..), other)
import Room (Room)
import qualified Room
import Scenario (Scenario (..))
import Server (addComputerClient, addPlayerClient, addSpecClient)
import qualified Server
import Start (startProgram)
import Stats.Progress (Progress (..), noProgress)
import System.Environment (lookupEnv)
import Text.Printf (printf)
import User (User (..), getUserFromCookies, getUsername, isSuperuser)
import Util (Gen, getGen, shuffle, to3Tuple)

main :: IO ()
main = do
  Log.setup
  loggerChan <- newChan
  loggerThreadID <- Log.forkLogger loggerChan
  let exceptionHandler = Log.errorChan loggerChan . show
  setUncaughtExceptionHandler exceptionHandler
  finally
    ( do
        -- If we're on production, these env vars will be present.
        -- Defined in `prod.env` secret.
        redisHost <- lookupEnv "REDIS_HOST"
        redisPort <- lookupEnv "REDIS_PORT"
        redisPassword <- lookupEnv "REDIS_PASSWORD"
        let redisVars = (redisHost, redisPort, redisPassword)

        postgresHost <- lookupEnv "POSTGRES_HOST"
        postgresPort <- lookupEnv "POSTGRES_PORT"
        postgresUser <- lookupEnv "POSTGRES_USER"
        postgresPassword <- lookupEnv "POSTGRES_PASSWORD"
        postgresDatabase <- lookupEnv "POSTGRES_DATABASE"
        let postgresVars = (postgresHost, postgresPort, postgresUser, postgresPassword, postgresDatabase)

        apiKey <- cs . fromMaybe "fake-api-key" <$> lookupEnv "GALGA_API_KEY"

        datadogKey <- lookupEnv "DD_API_KEY"
        datadogAppKey <- lookupEnv "DD_APP_KEY"

        let connectInfoConfig =
              ConnectInfoConfig
                { connectInfoConfig_redis = redisConnectInfo redisVars,
                  connectInfoConfig_postgres = postgresConnectInfo postgresVars,
                  connectInfoConfig_loggerChan = loggerChan,
                  connectInfoConfig_apiKey = apiKey,
                  connectInfoConfig_datadog = (cs <$> datadogAppKey, cs <$> datadogKey)
                }

        authApp <- runApp connectInfoConfig $ Auth.app connectInfoConfig
        state <- newTVarIO Server.initState

        Log.infoIO "Starting up!"

        let warpSettings =
              Warp.setOnException (const exceptionHandler) $
                Warp.setPort 9160 Warp.defaultSettings
        Warp.runSettings warpSettings $ waiApp state connectInfoConfig authApp
    )
    ( -- Cleanup loose threads. Useful when developing with repl.
      do
        Log.infoIO "Cleaning up!"
        killThread loggerThreadID
    )

waiApp :: TVar Server.State -> ConnectInfoConfig -> Application -> Application
waiApp state connectInfoConfig =
  websocketsOr
    WS.defaultConnectionOptions
    (wsApp state connectInfoConfig)

wsApp :: TVar Server.State -> ConnectInfoConfig -> WS.ServerApp
wsApp state connectInfoConfig pending = do
  let cookies = Auth.getCookies pending
  (acceptReq, cid) <- customAcceptRequest cookies
  connection <- WS.acceptRequestWith pending acceptReq
  WS.withPingThread
    connection
    30
    (return ())
    ( runApp connectInfoConfig $ do
        msg <- liftIO $ WS.receiveData connection
        user <- getUserFromCookies cookies cid
        begin connection msg user state
    )

customAcceptRequest :: Auth.Cookies -> IO (WS.AcceptRequest, Text)
customAcceptRequest cookies = do
  newCid <- GUID.genText
  let cid = fromMaybe newCid (Map.lookup Auth.cidCookieName cookies)
  let headers = [("Set-Cookie", cs $ Auth.cidCookieName <> "=" <> cid <> "; Path=/; Secure; SameSite=Strict")]
  let acceptReq = WS.defaultAcceptRequest {WS.acceptHeaders = headers}
  return (acceptReq, cs cid)

connectionFail :: WS.Connection -> String -> App ()
connectionFail conn str = do
  Log.warning str
  liftIO $ WS.sendTextData conn . Command.toChat . ErrorCommand $ cs str

begin :: WS.Connection -> Text -> User -> TVar Server.State -> App ()
begin conn request user state = do
  let username = getUsername user :: Text
  Log.info $ printf "<%s>: New connection" username
  case Negotiation.parseRequest request of
    Right (RoomRequest roomName) -> do
      Metrics.incr "request.room"
      Log.info $ printf "<%s>: Requesting room [%s]" username roomName
      msg <- liftIO $ WS.receiveData conn
      case Negotiation.parsePrefix msg of
        Nothing ->
          connectionFail conn $ printf "<%s>: Connection protocol failure" msg
        Just prefix -> do
          Log.info $ printf "<%s>: %s" username (show prefix)
          gen <- liftIO getGen
          guid <- liftIO GUID.genText
          let client = Client user (PlayerConnection conn) guid
          let scenario = makeScenario gen prefix
          roomVar <- liftIO . atomically $ Server.getOrCreateRoom roomName (prefixWaitType prefix) gen scenario state
          prefixMetric prefix
          beginPrefix prefix state client roomVar
    Right (SystemMessageRequest systemMessage) -> do
      Metrics.incr "request.systemMessage"
      if User.isSuperuser user
        then
          ( do
              Log.info $ printf ("System message received: " <> cs systemMessage)
              roomVars <- liftIO . atomically $ Server.getAllRooms state
              forM_
                roomVars
                ( \roomVar -> do
                    room <- liftIO $ readTVarIO roomVar
                    fork $ Room.broadcast ("systemMessage:" <> systemMessage) room
                )
              liftIO $ WS.sendTextData conn ("systemMessageSuccess:" :: Text)
              Log.info $ printf "System message success"
          )
        else Log.error $ printf "Illegal system message"
    Left Negotiation.ConnectionLostError -> do
      Metrics.incr "error.connectionLost"
      Log.warning $ printf "<%s>: Connection was lost, informing client" username
      liftIO $ WS.sendTextData conn ("connectionLost:" :: Text)
    Left (Negotiation.UnknownError err) -> do
      Metrics.incr "error.unknown"
      connectionFail conn $ printf "<%s>: %s" username err

beginPrefix :: Prefix -> TVar Server.State -> Client -> TVar Room -> App ()
beginPrefix PrefixPlay s c r = beginPlay s c r
beginPrefix PrefixSpec s c r = beginSpec s c r
beginPrefix PrefixQueue s c r = beginQueue s c r
beginPrefix PrefixCpu s c r = beginComputer "CPU" s c r >> return ()

prefixMetric :: Prefix -> App ()
prefixMetric PrefixPlay = Metrics.incr "begin.play"
prefixMetric PrefixSpec = Metrics.incr "begin.spec"
prefixMetric PrefixQueue = Metrics.incr "begin.queue"
prefixMetric PrefixCpu = Metrics.incr "begin.cpu"

prefixWaitType :: Prefix -> WaitType
prefixWaitType PrefixQueue = WaitQuickplay
prefixWaitType _ = WaitCustom

makeScenario :: Gen -> Prefix -> Scenario
makeScenario _ prefix =
  Scenario
    { scenario_turn = turn,
      scenario_characterPa = characterPa,
      scenario_characterPb = characterPb,
      scenario_prog = prog,
      scenario_progressWin = progressWin,
      scenario_progressLoss = progressLoss,
      scenario_timeLimit = timeLimit,
      scenario_tags = []
    }
  where
    characterPa :: Either UnchosenCharacter ChosenCharacter
    characterPa = Left $ UnchosenCharacter Nothing
    characterPb :: Either UnchosenCharacter ChosenCharacter
    characterPb = Left $ UnchosenCharacter Nothing
    turn :: Turn
    turn =
      case prefix of
        PrefixCpu ->
          PlayerB
        _ ->
          PlayerA
    prog :: Beta.Program ()
    prog = startProgram turn
    progressWin :: Progress
    progressWin = noProgress {progress_xp = 100}
    progressLoss :: Progress
    progressLoss = noProgress {progress_xp = 70}
    timeLimit :: NominalDiffTime
    timeLimit = fromIntegral (60 :: Integer)

beginPlay :: TVar Server.State -> Client -> TVar Room -> App ()
beginPlay state client roomVar = do
  Log.info $ printf "<%s>: Begin playing" (show $ Client.name client)
  added <- liftIO $ atomically $ addPlayerClient client roomVar
  case added of
    Nothing -> do
      Log.info $ printf "<%s>: Room is full" (show $ Client.name client)
      Client.send (Command.toChat $ ErrorCommand "Room is full") client
    Just (which, outcomes) ->
      finally
        ( do
            _ <- play which client roomVar outcomes
            return ()
        )
        (disconnect client roomVar state)

beginSpec :: TVar Server.State -> Client -> TVar Room -> App ()
beginSpec state client roomVar = do
  Log.info $ printf "<%s>: Begin spectating" (show $ Client.name client)
  finally
    (spectate client roomVar)
    (disconnect client roomVar state)

beginComputer :: Text -> TVar Server.State -> Client -> TVar Room -> App Bool
beginComputer cpuName state client roomVar = do
  Log.info $ printf "<%s>: Begin AI game" (show $ Client.name client)
  cpuGuid <- liftIO GUID.genText
  (computer, added) <- liftIO . atomically $ do
    progress <- Client.progress client
    computerAdded <- addComputerClient cpuName cpuGuid progress roomVar
    playerAdded <- addPlayerClient client roomVar
    return (computerAdded, playerAdded)
  case added of
    Just (which, outcomes) -> do
      case computer of
        Just computerClient -> do
          _ <- fork (computerPlay (other which) roomVar state computerClient)
          return ()
        Nothing ->
          return ()
      finally
        (play which client roomVar outcomes)
        (disconnect client roomVar state)
    Nothing -> do
      Log.info $ printf "<%s>: Room is full" (show $ Client.name client)
      Client.send (Command.toChat $ ErrorCommand "Room is full") client
      return False

beginQueue :: TVar Server.State -> Client -> TVar Room -> App ()
beginQueue state client roomVar = do
  let clientName = show (Client.name client)
  Log.info $ printf "<%s>: Begin quickplay game" clientName
  queueId <- liftIO GUID.genText
  finally
    ( do
        roomM <- liftIO . atomically $ Server.queue queueId roomVar state
        case roomM of
          Just existingRoom -> do
            room <- liftIO $ readTVarIO existingRoom
            let roomName = Room.getName room
            Log.info $ printf "<%s>: Joining quickplay room from queue [%s]" clientName roomName
            Client.send ("room:" <> roomName) client
            beginPlay state client existingRoom
          Nothing -> do
            room <- liftIO . readTVarIO $ roomVar
            let roomName = Room.getName room
            Log.info $ printf "<%s>: Using default quickplay room [%s]" clientName roomName
            Client.send ("room:" <> roomName) client
            asyncQueueCpuFallback state client roomVar queueId
            beginPlay state client roomVar
        gen <- liftIO getGen
        guid <- liftIO GUID.genText
        let scenario = makeScenario gen PrefixQueue
        let roomName = Text.take 8 guid
        newRoomVar <- liftIO . atomically $ Server.getOrCreateRoom roomName WaitQuickplay gen scenario state
        beginQueue state client newRoomVar
    )
    ( do
        -- If the client leaves, remove them from the queue.
        liftIO . atomically $ Server.dequeue queueId state
    )

data QueueCpuResult
  = QueueCpuNotNeeded
  | QueueCpuReconnect
  | QueueCpuFailed
  deriving (Show, Eq)

asyncQueueCpuFallback :: TVar Server.State -> Client -> TVar Room -> Text -> App ()
asyncQueueCpuFallback state client roomVar queueId = do
  _ <- fork $ do
    threadDelay (4 * 1000000)
    cpuGuid <- liftIO GUID.genText
    result <- liftIO $ atomically (transaction cpuGuid)
    case result of
      Left QueueCpuNotNeeded -> do
        Metrics.incr "quickplay.human"
        Log.info "Humans playing, no CPU needed"
      Left QueueCpuReconnect -> do
        Metrics.incr "quickplay.reconnect"
        Log.info "Reconnecting, no CPU needed"
      Left QueueCpuFailed -> do
        Metrics.incr "quickplay.fail"
        Log.info "Failed to add CPU"
      Right cpuClient -> do
        Metrics.incr "quickplay.cpu"
        Log.info $ printf "New CPU joining room"
        newRoom <- liftIO $ readTVarIO roomVar
        let gameState = Room.getState newRoom
        syncRoomMetadata newRoom
        syncClient client PlayerA gameState
        computerPlay PlayerB roomVar state cpuClient
  return ()
  where
    transaction :: Text -> STM (Either QueueCpuResult Client)
    transaction guid = do
      room <- readTVar roomVar
      if Room.full room
        then
          if Room.noCpus room
            then return $ Left QueueCpuNotNeeded
            else
              ( do
                  Server.dequeue queueId state
                  return $ Left QueueCpuReconnect
              )
        else
          ( do
              progress <- Client.progress client
              updateRoomEncounter roomVar progress
              added <- addComputerClient "CPU" guid progress roomVar
              case added of
                Just computerClient -> do
                  Server.dequeue queueId state
                  return $ Right computerClient
                Nothing ->
                  return $ Left QueueCpuFailed
          )

spectate :: Client -> TVar Room -> App ()
spectate client roomVar = do
  room <- liftIO . atomically $ addSpecClient client roomVar
  Client.send ("acceptSpec:" :: Text) client
  Room.broadcast (Command.toChat (SpectateCommand (Client.name client))) room
  syncClient client PlayerA (Room.getState room)
  syncRoomMetadata room
  _ <- runMaybeT . forever $ do
    msg <- lift $ Client.receive client
    lift $ actSpec (Command.parse (Client.name client) msg) roomVar
  return ()

play :: WhichPlayer -> Client -> TVar Room -> [Outcome] -> App Bool
play which client roomVar outcomes = do
  Client.send ("acceptPlay:" :: Text) client
  room <- liftIO $ readTVarIO roomVar
  syncRoomMetadata room
  let gameState = Room.getState room
  syncClient client which gameState
  forM_ outcomes (actOutcome room)
  _ <- runMaybeT . forever $ do
    msg <- lift $ Client.receive client
    let username = Client.name client
    let command = Command.parse username msg
    case command of
      EndEncounterCommand ->
        mzero -- Exit the loop, the encounter is over.
      _ ->
        lift $ actPlay command which roomVar username (Room.getName room)
  finalRoom <- liftIO $ readTVarIO roomVar
  return $ isWinner which $ Room.getState finalRoom

computerPlay :: WhichPlayer -> TVar Room -> TVar Server.State -> Client -> App ()
computerPlay which roomVar state client = do
  roomName <- liftIO $ Room.getName <$> readTVarIO roomVar
  progress <- liftIO $ atomically $ Client.progress client
  _ <- runMaybeT (runStateT (forever (loop roomName progress)) 0)
  Log.info $ printf "<%s@%s>AI signing off" (Client.name client) roomName
  _ <- disconnect client roomVar state
  return ()
  where
    loop :: Text -> Progress -> StateT Int (MaybeT App) ()
    loop roomName progress = do
      lift . lift $ threadDelay (1 * 1000000)
      gen <- liftIO getGen
      command <- lift . lift $ chooseComputerCommand which roomVar gen progress
      case command of
        Just c -> do
          lift . lift $ actPlay c which roomVar "CPU" roomName
        Nothing ->
          return ()
      room <- liftIO $ readTVarIO roomVar
      if Room.noHumans room
        then modify' (1 +)
        else put 0
      -- Break out if the room's been empty for ~1 minute.
      iterations <- get
      let timeout = 60
      -- when
      --   (iterations > 0)
      --   ( lift . lift . Log.debug $
      --       printf "<cpu@%s> CPU quitting in %d seconds" roomName (timeout - iterations)
      --   )
      when (iterations > timeout) mzero

chooseComputerCommand :: WhichPlayer -> TVar Room -> Gen -> Progress -> App (Maybe Command)
chooseComputerCommand which room gen progress = do
  r <- liftIO $ readTVarIO room
  case Room.getState r of
    Selecting deckModel _ _ ->
      if DeckBuilding.isReady deckModel which
        then return Nothing
        else return . Just . SelectCharacterCommand $ randomChoice
    Started (Playing playing) -> do
      let model = playing_model playing
      return $ trans <$> chooseAction gen which model (Room.getScenario r)
    _ ->
      return Nothing
  where
    trans :: Action -> Command
    trans EndAction = EndTurnCommand
    trans (PlayAction index) = PlayCardCommand index
    elligibleRunes :: [Rune]
    elligibleRunes = Set.toList $ progress_unlocks progress
    randomChoice :: DeckBuilding.CharacterChoice
    randomChoice = DeckBuilding.CharacterChoice a b c
    (a, b, c) = to3Tuple $ DeckBuilding.rune_name <$> shuffle gen elligibleRunes

disconnect :: Client -> TVar Room -> TVar Server.State -> App Server.State
disconnect client roomVar state = do
  room <- liftIO . atomically $ Server.removeClient client roomVar
  Room.broadcast (Command.toChat . LeaveCommand $ Client.name client) room
  syncRoomMetadata room
  if Room.empty room
    then
      ( do
          Log.info $ printf "<%s>: Room is empty, deleting room" (show $ Client.name client)
          liftIO $ atomically $ Server.deleteRoom (Room.getName room) state
      )
    else
      ( do
          Log.info $ printf "<%s>: Room is not empty, retaining room" (show $ Client.name client)
          liftIO $ readTVarIO state
      )

disconnectComputers :: TVar Room -> TVar Server.State -> App ()
disconnectComputers roomVar state = do
  room <- liftIO $ readTVarIO roomVar
  let clients = Room.getClients room
  let computerClients = filter Client.isCpu clients
  forM_ computerClients (\client -> disconnect client roomVar state)

runRepl :: App a -> IO a
runRepl app = do
  Log.setup
  loggerChan <- newChan
  let connectInfoConfig =
        ConnectInfoConfig
          { connectInfoConfig_redis = redisConnectInfo (Nothing, Nothing, Nothing),
            connectInfoConfig_postgres = postgresConnectInfo (Nothing, Nothing, Nothing, Nothing, Nothing),
            connectInfoConfig_loggerChan = loggerChan,
            connectInfoConfig_apiKey = "fake-api-key",
            connectInfoConfig_datadog = (Nothing, Nothing)
          }
  runApp connectInfoConfig app
