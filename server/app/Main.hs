module Main where

import Control.Concurrent.Lifted (fork, killThread, threadDelay)
import Control.Monad (forM_)
import Control.Monad.STM (STM, atomically)
import Control.Concurrent.Chan (newChan)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, readTVarIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Exception.Lifted (finally)
import Control.Monad (forever, mzero, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Time.Clock (NominalDiffTime)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Text.Printf (printf)
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

import Act (actOutcome, actPlay, actSpec, syncPlayersRoom, syncClient)
import ArtificialIntelligence (Action(..), chooseAction)
import Config (App, ConnectInfoConfig(..), runApp)
import Database (postgresConnectInfo, redisConnectInfo)
import GameState (GameState(..), PlayState(..), PlayingR(..), WaitType(..), isWinner)
import Model (Turn)
import Outcome (Outcome)
import Player (WhichPlayer(..), other)
import Scenario (Scenario(..))
import Start (startProgram)
import Stats.Stats (Experience)
import User (User(..), getUsername, getUserFromCookies, isSuperuser)
import Util (Gen, getGen, shuffle, split, to3Tuple)

import qualified DSL.Beta as Beta

import qualified Server
import Server (addComputerClient, addPlayerClient, addSpecClient)

import qualified DeckBuilding
import DeckBuilding (Character(..), ChosenCharacter(..), UnchosenCharacter(..))

import qualified Client
import Client (Client(..), ClientConnection(..))

import qualified Command
import Command (Command(..))

import qualified Negotiation
import Negotiation (Prefix(..), Request(..))

import qualified Log

import qualified Room
import Room (Room)

import qualified Data.Text as Text
import Data.Text (Text)

import qualified Replay.Final

import qualified Network.WebSockets as WS

import qualified Auth.Apps as Auth
import qualified Auth.Views as Auth
import qualified Data.GUID as GUID


main :: IO ()
main = do
  Log.setup
  loggerChan <- newChan
  loggerThreadID <- Log.forkLogger loggerChan
  finally (do
    -- If we're on production, these env vars will be present.
    -- Defined in `prod.env` secret.
    redisHost     <- lookupEnv "REDIS_HOST"
    redisPort     <- lookupEnv "REDIS_PORT"
    redisPassword <- lookupEnv "REDIS_PASSWORD"
    let redisVars = (redisHost, redisPort, redisPassword)

    postgresHost     <- lookupEnv "POSTGRES_HOST"
    postgresPort     <- lookupEnv "POSTGRES_PORT"
    postgresUser     <- lookupEnv "POSTGRES_USER"
    postgresPassword <- lookupEnv "POSTGRES_PASSWORD"
    postgresDatabase <- lookupEnv "POSTGRES_DATABASE"
    let postgresVars = (postgresHost, postgresPort, postgresUser, postgresPassword, postgresDatabase)

    apiKey <- cs <$> fromMaybe "fake-api-key" <$> lookupEnv "GALGA_API_KEY"

    let connectInfoConfig = ConnectInfoConfig
                              { connectInfoConfig_redis      = redisConnectInfo redisVars
                              , connectInfoConfig_postgres   = postgresConnectInfo postgresVars
                              , connectInfoConfig_loggerChan = loggerChan
                              , connectInfoConfig_apiKey     = apiKey
                              }

    authApp <- runApp connectInfoConfig $ Auth.app connectInfoConfig
    state <- atomically $ newTVar Server.initState

    Log.infoIO "Starting up!"

    run 9160 $ waiApp state connectInfoConfig authApp
    ) (-- Cleanup loose threads. Useful when developing with repl.
      do
        Log.infoIO "Cleaning up!"
        killThread loggerThreadID
    )


waiApp :: TVar Server.State -> ConnectInfoConfig -> Application -> Application
waiApp state connectInfoConfig backupApp =
  websocketsOr
    WS.defaultConnectionOptions
      (wsApp state connectInfoConfig)
      backupApp


wsApp :: TVar Server.State -> ConnectInfoConfig -> WS.ServerApp
wsApp state connectInfoConfig pending =
  runApp connectInfoConfig $ do
    connection <- liftIO $ WS.acceptRequest pending
    msg <- liftIO $ WS.receiveData connection
    user <- getUserFromCookies $ Auth.getCookies pending
    liftIO $ WS.forkPingThread connection 30
    begin connection msg user state


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
          beginPrefix prefix state client roomVar
    Right (PlayReplayRequest replayId) -> do
      mReplay <- Replay.Final.load replayId
      case mReplay of
        Just replay ->
          liftIO $ WS.sendTextData conn ("replay:" <> replay)
        Nothing ->
          liftIO $ WS.sendTextData conn ("replayNotFound:" :: Text)
    Right (SystemMessageRequest systemMessage) ->
      case User.isSuperuser user of
        True -> do
          Log.info $ printf ("System message received: " <> cs systemMessage)
          roomVars <- liftIO . atomically $ Server.getAllRooms state
          forM_ roomVars (
            \roomVar -> do
              room <- liftIO $ readTVarIO roomVar
              fork $ Room.broadcast ("systemMessage:" <> systemMessage) room
            )
          liftIO $ WS.sendTextData conn ("systemMessageSuccess:" :: Text)
          Log.info $ printf ("System message success")
        False ->
          Log.error $ printf "Illegal system message"
    Left Negotiation.ConnectionLostError -> do
      Log.warning $ printf "<%s>: Connection was lost, informing client" username
      liftIO $ WS.sendTextData conn ("connectionLost:" :: Text)
    Left (Negotiation.UnknownError err) ->
      connectionFail conn $ printf "<%s>: %s" username err


beginPrefix :: Prefix -> TVar Server.State -> Client -> TVar Room -> App ()
beginPrefix PrefixPlay  s c r = beginPlay s c r
beginPrefix PrefixSpec  s c r = beginSpec s c r
beginPrefix PrefixQueue s c r = beginQueue s c r
beginPrefix PrefixCpu   s c r = beginComputer "CPU" s c r >> return ()


prefixWaitType :: Prefix -> WaitType
prefixWaitType PrefixQueue = WaitQuickplay
prefixWaitType _           = WaitCustom


makeScenario :: Gen -> Prefix -> Scenario
makeScenario gen prefix =
  Scenario {
    scenario_turn = turn
  , scenario_characterPa = characterPa
  , scenario_characterPb = characterPb
  , scenario_prog = prog
  , scenario_xpWin = xpWin
  , scenario_xpLoss = xpLoss
  , scenario_timeLimit = timeLimit
  }
  where
    (genA, genB) = split gen
    characterPa :: Either UnchosenCharacter ChosenCharacter
    characterPa = Left . UnchosenCharacter $ Character (Left $ DeckBuilding.randomRunes genA) 50
    characterPb :: Either UnchosenCharacter ChosenCharacter
    characterPb = Left . UnchosenCharacter $ Character (Left $ DeckBuilding.randomRunes genB) 50
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
  added <- liftIO $  atomically $ addPlayerClient client roomVar
  case added of
    Nothing -> do
      Log.info $ printf "<%s>: Room is full" (show $ Client.name client)
      Client.send (Command.toChat $ ErrorCommand "room is full") client
    Just (which, outcomes) ->
      finally
        (do
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
    computerAdded <- addComputerClient cpuName cpuGuid roomVar
    playerAdded <- addPlayerClient client roomVar
    return (computerAdded, playerAdded)
  case (,) <$> computer <*> added of
    Nothing -> do
      Log.info $ printf "<%s>: Room is full" (show $ Client.name client)
      Client.send (Command.toChat $ ErrorCommand "Room is full") client
      return False
    Just (computerClient, (which, outcomes)) ->
      finally
        (do
          _ <- fork (computerPlay (other which) roomVar)
          play which client roomVar outcomes
        )
        (do
          _ <- disconnect computerClient roomVar state
          (disconnect client roomVar state)
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
        (do
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
      Left toLog ->
        Log.info toLog
      Right _ -> do
        newRoom <- liftIO $ readTVarIO roomVar
        let gameState = Room.getState newRoom
        syncClient client PlayerA gameState
        syncPlayersRoom newRoom
        computerPlay PlayerB roomVar
  return ()
  where
    transaction :: Text -> STM (Either String Client)
    transaction guid = do
      room <- readTVar roomVar
      case Room.full room of
        True -> do
          return $ Left "No CPU needed for queue, rejoice"
        False -> do
          added <- addComputerClient "CPU" guid roomVar
          case added of
            Just computerClient -> do
              Server.dequeue state
              return $ Right computerClient
            Nothing ->
              return $ Left "Failed to add CPU"


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


computerPlay :: WhichPlayer -> TVar Room -> App ()
computerPlay which roomVar = do
  roomName <- liftIO $ Room.getName <$> readTVarIO roomVar
  _ <- runMaybeT . forever $ loop roomName
  Log.info $ printf "AI signing off"
  return ()
  where
    loop :: Text -> MaybeT App ()
    loop roomName = do
      lift $ threadDelay 1000000
      gen <- liftIO $ getGen
      command <- lift $ chooseComputerCommand which roomVar gen
      case command of
        Just c -> do
          lift $ actPlay c which roomVar "CPU" roomName
          lift $ threadDelay 10000
        Nothing ->
          return ()
      -- Break out if the room's empty.
      room <- liftIO $ readTVarIO roomVar
      when (Room.empty room) mzero


chooseComputerCommand :: WhichPlayer -> TVar Room -> Gen -> App (Maybe Command)
chooseComputerCommand which room gen = do
  r <- liftIO $ readTVarIO room
  case Room.getState r of
    Selecting deckModel _ _ ->
      if DeckBuilding.isReady deckModel which then
        return Nothing
          else
            return . Just . SelectCharacterCommand $ randomChoice
    Started (Playing playing) -> do
      let model = playing_model playing
      return $ trans <$> chooseAction gen which model (Room.getScenario r)
    _ ->
      return Nothing
  where
    trans :: Action -> Command
    trans EndAction          = EndTurnCommand
    trans (PlayAction index) = PlayCardCommand index
    randomChoice :: DeckBuilding.CharacterChoice
    randomChoice = DeckBuilding.CharacterChoice a b c
    (a, b, c) = to3Tuple $ DeckBuilding.rune_name <$> shuffle gen DeckBuilding.mainRunes


disconnect :: Client -> TVar Room -> TVar Server.State -> App (Server.State)
disconnect client roomVar state = do
  room <- liftIO . atomically $ Server.removeClient client roomVar
  Room.broadcast (Command.toChat . LeaveCommand $ Client.name client) room
  syncPlayersRoom room
  if Room.empty room then
    (do
      Log.info $ printf "<%s>: Room is empty, deleting room" (show $ Client.name client)
      liftIO $ atomically $ Server.deleteRoom (Room.getName room) state
    )
      else
        (do
          Log.info $ printf "<%s>: Room is not empty, retaining room" (show $ Client.name client)
          liftIO $ readTVarIO state
        )


disconnectComputers :: TVar Room -> TVar Server.State -> App ()
disconnectComputers roomVar state = do
  room <- liftIO $ readTVarIO roomVar
  let clients = Room.getClients room
  let computerClients = filter Client.isCpu clients
  forM_ computerClients (\client -> disconnect client roomVar state)
