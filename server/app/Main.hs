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
import qualified Presence.Apps as Presence
import qualified Presence.Presence as Presence
import Room (Room)
import qualified Room
import Scenario (Scenario (..), applyCustomSettings)
import Server (addComputerClient, addPlayerClient, addSpecClient)
import qualified Server
import Start (roundEndProgram, startProgram)
import Stats.Progress (Progress (..), isTutorialProgress)
import System.Environment (lookupEnv)
import Text.Printf (printf)
import User.Apps (getUserFromCookies)
import User.User (getUsername)
import qualified User.User as User
import Util (Gen, forkDelay, getGen, shuffle, to3Tuple)

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

        presenceVar <- atomically Presence.new

        let connectInfoConfig =
              ConnectInfoConfig
                { connectInfoConfig_redis = redisConnectInfo redisVars,
                  connectInfoConfig_postgres = postgresConnectInfo postgresVars,
                  connectInfoConfig_loggerChan = loggerChan,
                  connectInfoConfig_apiKey = apiKey,
                  connectInfoConfig_datadog = (cs <$> datadogAppKey, cs <$> datadogKey),
                  connectInfoConfig_presenceVar = presenceVar
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
  conn <- WS.acceptRequestWith pending acceptReq
  WS.withPingThread
    conn
    30
    (return ())
    ( runApp connectInfoConfig $ do
        user <- getUserFromCookies cookies cid
        guid <- liftIO GUID.genText
        let client = Client user (PlayerConnection conn) guid
        finally
          ( do
              Presence.add client
              begin client state
          )
          (Presence.remove client)
    )

customAcceptRequest :: Auth.Cookies -> IO (WS.AcceptRequest, Text)
customAcceptRequest cookies = do
  newCid <- GUID.genText
  let cid = fromMaybe newCid (Map.lookup Auth.cidCookieName cookies)
  let headers = [("Set-Cookie", cs $ Auth.cidCookieName <> "=" <> cid <> "; Path=/; Secure; SameSite=Strict")]
  let acceptReq = WS.defaultAcceptRequest {WS.acceptHeaders = headers}
  return (acceptReq, cs cid)

connectionFail :: Client -> String -> App ()
connectionFail client str = do
  Log.warning str
  liftIO $ Client.send (Command.toChat . ErrorCommand $ cs str) client

begin :: Client -> TVar Server.State -> App ()
begin client state = do
  let user = Client.user client
  let username = getUsername user :: Text
  Log.info $ printf "<%s>: New connection" username
  request <- liftIO $ Client.receive client
  case Negotiation.parseRequest request of
    Right (RoomRequest roomName mCustomSettings) -> do
      Metrics.incr "request.room"
      Log.info $ printf "<%s>: Requesting room [%s]" username roomName
      msg <- liftIO $ Client.receive client
      case Negotiation.parsePrefix msg of
        Nothing ->
          connectionFail client $ printf "<%s>: Connection protocol failure" msg
        Just prefix -> do
          Log.info $ printf "<%s>: %s" username (show prefix)
          gen <- liftIO getGen
          let scenario = applyCustomSettings mCustomSettings $ makeScenario gen
          roomVar <- liftIO . atomically $ Server.getOrCreateRoom roomName (prefixWaitType prefix) gen scenario state
          prefixMetric prefix
          beginPrefix prefix state client roomVar
    Right (ChallengeRequest opponentId roomName) -> do
      Log.info $ printf "<%s>: Challenging <id:%d>@[%s]" username opponentId roomName
      mOpponentClient <- Presence.get opponentId
      case mOpponentClient of
        Just opponentClient -> do
          -- Challenge opponent.
          liftIO $ Client.send ("challengedBy:" <> "username" <> "," <> roomName) opponentClient
          Log.info $ printf "<%s>: Challenged <id:%d>@[%s]" username opponentId roomName
          -- Setup game.
          liftIO $ Client.send ("challengeRoom:" <> roomName) client
          Metrics.incr "request.challenge"
          gen <- liftIO getGen
          let scenario = makeScenario gen
          let prefix = PrefixPlay
          roomVar <- liftIO . atomically $ Server.getOrCreateRoom roomName (prefixWaitType prefix) gen scenario state
          prefixMetric prefix
          beginPrefix prefix state client roomVar
        Nothing ->
          Log.error $ printf "<%s>: Cannot find opponent <id:%d>" username opponentId
    Right (SystemMessageRequest systemMessage) -> do
      Metrics.incr "request.systemMessage"
      if User.isSuperuser user
        then
          ( do
              Log.info $ printf ("System message received: " <> cs systemMessage)
              rooms <- liftIO . atomically $ Server.getAllRooms state
              forM_ rooms (fork . Room.broadcast ("systemMessage:" <> systemMessage))
              liftIO $ Client.send "systemMessageSuccess:" client
              Log.info $ printf "System message success"
          )
        else Log.error $ printf "Illegal system message"
    Left Negotiation.ConnectionLostError -> do
      Metrics.incr "error.connectionLost"
      Log.warning $ printf "<%s>: Connection was lost, informing client" username
      liftIO $ Client.send "connectionLost:" client
    Left (Negotiation.UnknownError err) -> do
      Metrics.incr "error.unknown"
      connectionFail client $ printf "<%s>: %s" username err

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

makeScenario :: Gen -> Scenario
makeScenario _ =
  Scenario
    { scenario_turn = turn,
      scenario_characterPa = characterPa,
      scenario_characterPb = characterPb,
      scenario_prog = prog,
      scenario_roundEndProg = roundEndProg,
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
    turn :: Maybe Turn
    turn = Nothing
    prog :: Maybe (Text, Text) -> Beta.Program ()
    prog = startProgram
    roundEndProg :: Beta.Program ()
    roundEndProg = roundEndProgram
    progressWin :: Progress
    progressWin = mempty {progress_xp = 100}
    progressLoss :: Progress
    progressLoss = mempty {progress_xp = 70}
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
        Just (computerClient, computerOutcomes) -> do
          _ <- fork (computerPlay (other which) roomVar state computerClient computerOutcomes)
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
  progress <- liftIO $ atomically $ Client.progress client
  let isTutorial = isTutorialProgress progress
  finally
    ( do
        roomM <-
          if isTutorial
            then return Nothing
            else liftIO . atomically $ Server.queue queueId roomVar state
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
            let delay = if isTutorial then 1 else 4
            forkDelay (delay * 1000000) (queueCpuFallback state client roomVar queueId)
            beginPlay state client roomVar
    )
    ( do
        -- If the client leaves, remove them from the queue.
        liftIO . atomically $ Server.dequeue queueId state
    )
  gen <- liftIO getGen
  guid <- liftIO GUID.genText
  let scenario = makeScenario gen
  let roomName = Text.take 8 guid
  newRoomVar <- liftIO . atomically $ Server.getOrCreateRoom roomName WaitQuickplay gen scenario state
  beginQueue state client newRoomVar

data QueueCpuResult
  = QueueCpuNotNeeded
  | QueueCpuReconnect
  | QueueCpuFailed
  deriving (Show, Eq)

queueCpuFallback :: TVar Server.State -> Client -> TVar Room -> Text -> App ()
queueCpuFallback state client roomVar queueId = do
  cpuGuid <- liftIO GUID.genText
  gen <- liftIO getGen
  result <- liftIO $ atomically (transaction cpuGuid gen)
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
    Right (cpuClient, outcomes) -> do
      Metrics.incr "quickplay.cpu"
      Log.info $ printf "New CPU joining room"
      newRoom <- liftIO $ readTVarIO roomVar
      let gameState = Room.getState newRoom
      syncRoomMetadata newRoom
      syncClient client PlayerA gameState
      computerPlay PlayerB roomVar state cpuClient outcomes
  where
    transaction :: Text -> Gen -> STM (Either QueueCpuResult (Client, [Outcome]))
    transaction guid gen = do
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
              updateRoomEncounter roomVar progress gen
              added <- addComputerClient "CPU" guid progress roomVar
              case added of
                Just (computerClient, outcomes) -> do
                  Server.dequeue queueId state
                  return $ Right (computerClient, outcomes)
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

computerPlay :: WhichPlayer -> TVar Room -> TVar Server.State -> Client -> [Outcome] -> App ()
computerPlay which roomVar state client outcomes = do
  room <- liftIO $readTVarIO roomVar
  let roomName = Room.getName room
  progress <- liftIO $ atomically $ Client.progress client
  forM_ outcomes (actOutcome room)
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
  presenceVar <- atomically Presence.new
  let connectInfoConfig =
        ConnectInfoConfig
          { connectInfoConfig_redis = redisConnectInfo (Nothing, Nothing, Nothing),
            connectInfoConfig_postgres = postgresConnectInfo (Nothing, Nothing, Nothing, Nothing, Nothing),
            connectInfoConfig_loggerChan = loggerChan,
            connectInfoConfig_apiKey = "fake-api-key",
            connectInfoConfig_datadog = (Nothing, Nothing),
            connectInfoConfig_presenceVar = presenceVar
          }
  runApp connectInfoConfig app
