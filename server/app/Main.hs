module Main where

import Act (actOutcome, actPlay, actSpec, syncClient, syncPlayersRoom)
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
import qualified DSL.Beta as Beta
import qualified Data.GUID as GUID
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
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
import qualified Replay.Final
import Room (Room)
import qualified Room
import Scenario (Scenario (..))
import Server (addComputerClient, addPlayerClient, addSpecClient)
import qualified Server
import Start (startProgram)
import Stats.Experience (Experience, nextLevelExperience)
import System.Environment (lookupEnv)
import Text.Printf (printf)
import User (User (..), getUserFromCookies, getUsername, isSuperuser)
import Util (Gen, getGen, shuffle, to3Tuple)

main :: IO ()
main = do
  Log.setup
  loggerChan <- newChan
  loggerThreadID <- Log.forkLogger loggerChan
  let exceptionHandler = (Log.errorChan loggerChan . show)
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
    Right (PlayReplayRequest replayId) -> do
      Metrics.incr "request.replay"
      Log.info $ printf "<%s>: watching replay %s" username (show replayId)
      mReplay <- Replay.Final.load (fromIntegral replayId)
      case mReplay of
        Just replay -> do
          liftIO $ WS.sendTextData conn ("replay:" <> replay)
        Nothing -> do
          liftIO $ WS.sendTextData conn ("replayNotFound:" :: Text)
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
      scenario_xpWin = xpWin,
      scenario_xpLoss = xpLoss,
      scenario_timeLimit = timeLimit
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
    xpWin :: Experience
    xpWin = 100
    xpLoss :: Experience
    xpLoss = 70
    timeLimit :: NominalDiffTime
    timeLimit = fromIntegral (60 :: Integer)

beginPlay :: TVar Server.State -> Client -> TVar Room -> App ()
beginPlay state client roomVar = do
  Log.info $ printf "<%s>: Begin playing" (show $ Client.name client)
  added <- liftIO $ atomically $ addPlayerClient client roomVar
  case added of
    Nothing -> do
      Log.info $ printf "<%s>: Room is full" (show $ Client.name client)
      Client.send (Command.toChat $ ErrorCommand "room is full") client
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
    xp <- Client.xp client
    computerAdded <- addComputerClient cpuName cpuGuid xp roomVar
    playerAdded <- addPlayerClient client roomVar
    return (computerAdded, playerAdded)
  case (,) <$> computer <*> added of
    Nothing -> do
      Log.info $ printf "<%s>: Room is full" (show $ Client.name client)
      Client.send (Command.toChat $ ErrorCommand "Room is full") client
      return False
    Just (computerClient, (which, outcomes)) ->
      finally
        ( do
            _ <- fork (computerPlay (other which) roomVar computerClient)
            play which client roomVar outcomes
        )
        ( do
            _ <- disconnect computerClient roomVar state
            disconnect client roomVar state
        )

beginQueue :: TVar Server.State -> Client -> TVar Room -> App ()
beginQueue state client roomVar = do
  let clientName = show (Client.name client)
  Log.info $ printf "<%s>: Begin quickplay game" clientName
  roomM <- liftIO . atomically $ Server.queue roomVar state
  case roomM of
    Just existingRoom -> do
      Log.info $ printf "<%s>: Joining existing quickplay room" clientName
      beginPlay state client existingRoom
    Nothing -> do
      room <- liftIO . readTVarIO $ roomVar
      Log.info $ printf "<%s>: Creating new quickplay room [%s]" clientName (Room.getName room)
      finally
        ( do
            asyncQueueCpuFallback state client roomVar
            beginPlay state client roomVar
        )
        (disconnectComputers roomVar state)
  gen <- liftIO getGen
  guid <- liftIO GUID.genText
  let scenario = makeScenario gen PrefixQueue
  let roomName = Text.take 8 guid
  newRoomVar <- liftIO . atomically $ Server.getOrCreateRoom roomName WaitQuickplay gen scenario state
  beginQueue state client newRoomVar

asyncQueueCpuFallback :: TVar Server.State -> Client -> TVar Room -> App ()
asyncQueueCpuFallback state client roomVar = do
  _ <- fork $ do
    threadDelay (4 * 1000000)
    cpuGuid <- liftIO GUID.genText
    result <- liftIO $ atomically (transaction cpuGuid)
    case result of
      Left toLog -> do
        Metrics.incr "quickplay.human"
        Log.info toLog
      Right cpuClient -> do
        Metrics.incr "quickplay.cpu"
        newRoom <- liftIO $ readTVarIO roomVar
        let gameState = Room.getState newRoom
        syncClient client PlayerA gameState
        syncPlayersRoom newRoom
        computerPlay PlayerB roomVar cpuClient
  return ()
  where
    transaction :: Text -> STM (Either String Client)
    transaction guid = do
      room <- readTVar roomVar
      if Room.full room
        then
          ( do
              return $ Left "No CPU needed for queue, rejoice"
          )
        else
          ( do
              xp <- Client.xp client
              updateRoomEncounter roomVar xp
              added <- addComputerClient "CPU" guid (nextLevelExperience xp) roomVar
              case added of
                Just computerClient -> do
                  Server.dequeue state
                  return $ Right computerClient
                Nothing ->
                  return $ Left "Failed to add CPU"
          )

spectate :: Client -> TVar Room -> App ()
spectate client roomVar = do
  room <- liftIO . atomically $ addSpecClient client roomVar
  Client.send ("acceptSpec:" :: Text) client
  Room.broadcast (Command.toChat (SpectateCommand (Client.name client))) room
  syncClient client PlayerA (Room.getState room)
  syncPlayersRoom room
  _ <- runMaybeT . forever $ do
    msg <- lift $ Client.receive client
    lift $ actSpec (Command.parse (Client.name client) msg) roomVar
  return ()

play :: WhichPlayer -> Client -> TVar Room -> [Outcome] -> App Bool
play which client roomVar outcomes = do
  Client.send ("acceptPlay:" :: Text) client
  room <- liftIO $ readTVarIO roomVar
  syncPlayersRoom room
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

computerPlay :: WhichPlayer -> TVar Room -> Client -> App ()
computerPlay which roomVar client = do
  roomName <- liftIO $ Room.getName <$> readTVarIO roomVar
  xp <- liftIO $ atomically $ Client.xp client
  _ <- runMaybeT . forever $ loop roomName xp
  Log.info $ printf "AI signing off"
  return ()
  where
    loop :: Text -> Experience -> MaybeT App ()
    loop roomName xp = do
      lift $ threadDelay 1000000
      gen <- liftIO getGen
      command <- lift $ chooseComputerCommand which roomVar gen xp
      case command of
        Just c -> do
          lift $ actPlay c which roomVar "CPU" roomName
          lift $ threadDelay 10000
        Nothing ->
          return ()
      -- Break out if the room's empty.
      room <- liftIO $ readTVarIO roomVar
      when (Room.empty room) mzero

chooseComputerCommand :: WhichPlayer -> TVar Room -> Gen -> Experience -> App (Maybe Command)
chooseComputerCommand which room gen xp = do
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
    elligibleRunes = filter (\rune -> rune_xp rune <= xp) DeckBuilding.mainRunes
    randomChoice :: DeckBuilding.CharacterChoice
    randomChoice = DeckBuilding.CharacterChoice a b c
    (a, b, c) = to3Tuple $ DeckBuilding.rune_name <$> shuffle gen elligibleRunes

disconnect :: Client -> TVar Room -> TVar Server.State -> App Server.State
disconnect client roomVar state = do
  room <- liftIO . atomically $ Server.removeClient client roomVar
  Room.broadcast (Command.toChat . LeaveCommand $ Client.name client) room
  syncPlayersRoom room
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
