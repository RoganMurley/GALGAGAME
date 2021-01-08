module Main where

import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Monad (forM_)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVarIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Exception.Lifted (finally)
import Control.Monad (forever, mzero, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (encode)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Text.Printf (printf)
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

import Act (actOutcome, actPlay, actSpec, syncClient, syncPlayersRoom, syncClient)
import ArtificalIntelligence (Action(..), chooseAction)
import Config (App, ConnectInfoConfig(..), runApp)
import Database (postgresConnectInfo, redisConnectInfo)
import GameState (GameState(..), PlayState(..), WaitType(..), isWinner)
import Model (Turn)
import Negotiation (Prefix(..), Request(..), parseRequest, parsePrefix)
import Outcome (Outcome)
import Player (WhichPlayer(..), other)
import Scenario (Scenario(..))
import Start (startProgram, tutorialStartProgram)
import Stats.Stats (Experience)
import User (User(..), getUsername, getUserFromToken, isSuperuser)
import Util (Gen, getGen, shuffle, split)

import qualified DSL.Beta as Beta

import qualified Server
import Server (addComputerClient, addPlayerClient, addSpecClient)

import qualified DeckBuilding

import qualified Client
import Client (Client(..), ClientConnection(..))

import qualified Command
import Command (Command(..))

import qualified Log

import qualified Room
import Room (Room)

import qualified Replay.Final

import qualified World.World as World

import qualified Network.WebSockets as WS

import qualified Auth.Apps as Auth
import qualified Auth.Views as Auth
import qualified Data.GUID as GUID


main :: IO ()
main = do
  Log.setup

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

  let connectInfoConfig = ConnectInfoConfig (redisConnectInfo redisVars) (postgresConnectInfo postgresVars)

  authApp   <- runApp connectInfoConfig $ Auth.app connectInfoConfig
  state     <- atomically $ newTVar Server.initState

  Log.info "Starting up!"
  run 9160 $ waiApp state connectInfoConfig authApp


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
    msg        <- liftIO $ WS.receiveData connection
    user       <- getUserFromToken $ Auth.getToken pending
    liftIO $ WS.forkPingThread connection 30
    begin connection msg user state


connectionFail :: WS.Connection -> String -> App ()
connectionFail conn str =
  liftIO $ do
    Log.warning str
    WS.sendTextData conn . Command.toChat . ErrorCommand $ cs str


begin :: WS.Connection -> Text -> User -> TVar Server.State -> App ()
begin conn request user state = do
  let username = getUsername user :: Text
  liftIO $ Log.info $ printf "<%s>: New connection" username
  case parseRequest request of
    Just (RoomRequest roomName) -> do
      liftIO $ Log.info $ printf "<%s>: Requesting room [%s]" username roomName
      msg <- liftIO $ WS.receiveData conn
      case parsePrefix msg of
        Nothing ->
          connectionFail conn $ printf "<%s>: Connection protocol failure" msg
        Just prefix -> do
          liftIO $ Log.info $ printf "<%s>: %s" username (show prefix)
          gen <- liftIO getGen
          guid <- liftIO GUID.genText
          let client = Client user (PlayerConnection conn) guid
          let scenario = makeScenario prefix
          roomVar <- liftIO . atomically $ Server.getOrCreateRoom roomName (prefixWaitType prefix) gen scenario state
          beginPrefix prefix state client roomVar
    Just (PlayReplayRequest replayId) -> do
      mReplay <- Replay.Final.load replayId
      case mReplay of
        Just replay ->
          liftIO $ WS.sendTextData conn ("replay:" <> replay)
        Nothing ->
          liftIO $ WS.sendTextData conn ("replayNotFound:" :: Text)
    Just (SystemMessageRequest systemMessage) ->
      case User.isSuperuser user of
        True -> do
          liftIO $ Log.info $ printf ("System message received: " <> cs systemMessage)
          roomVars <- liftIO . atomically $ Server.getAllRooms state
          forM_ roomVars (
            \roomVar -> do
              room <- liftIO $ readTVarIO roomVar
              fork $ Room.broadcast ("systemMessage:" <> systemMessage) room
            )
          liftIO $ WS.sendTextData conn ("systemMessageSuccess:" :: Text)
          liftIO $ Log.info $ printf ("System message success")
        False ->
          liftIO $ Log.error $ printf "Illegal system message"
    Just WorldRequest -> do
      liftIO $ Log.info $ printf "<%s>: Visting World" username
      guid <- liftIO GUID.genText
      let client = Client user (PlayerConnection conn) guid
      beginWorld state client Nothing
    Nothing ->
      connectionFail conn $ printf "<%s>: Bad request %s" (show username) request


beginPrefix :: Prefix -> TVar Server.State -> Client -> TVar Room -> App ()
beginPrefix PrefixPlay     s c r = beginPlay s c r
beginPrefix PrefixSpec     s c r = beginSpec s c r
beginPrefix PrefixQueue    s c r = beginQueue s c r
beginPrefix PrefixCpu      s c r = beginComputer "CPU" s c r >> return ()
beginPrefix PrefixTutorial s c r = beginComputer "CPU" s c r >> return ()
beginPrefix PrefixDaily    s c r = beginComputer "CPU" s c r >> return ()
beginPrefix PrefixWorld    s c r = beginComputer "CPU" s c r >> return ()


prefixWaitType :: Prefix -> WaitType
prefixWaitType PrefixQueue = WaitQuickplay
prefixWaitType _           = WaitCustom


makeScenario :: Prefix -> Scenario
makeScenario prefix =
  Scenario {
    scenario_turn = turn
  , scenario_characterPa = characterPa
  , scenario_characterPb = characterPb
  , scenario_prog = prog
  , scenario_xpWin = xpWin
  , scenario_xpLoss = xpLoss
  }
  where
    characterPa :: Maybe DeckBuilding.Character
    characterPa =
      case prefix of
        PrefixTutorial ->
          Just DeckBuilding.catherine
        PrefixDaily ->
          Just DeckBuilding.marcus
        _ ->
          Nothing
    characterPb :: Maybe DeckBuilding.Character
    characterPb =
      case prefix of
        PrefixTutorial ->
          Just DeckBuilding.marcus
        PrefixDaily ->
          Just DeckBuilding.catherine
        _ ->
          Nothing
    turn :: Turn
    turn =
      case prefix of
        PrefixCpu ->
          PlayerB
        PrefixTutorial ->
          PlayerB
        PrefixDaily ->
          PlayerB
        _ ->
          PlayerA
    prog :: Beta.Program ()
    prog =
      case prefix of
        PrefixTutorial -> do
          tutorialStartProgram turn
        _ ->
          startProgram turn
    xpWin :: Experience
    xpWin = 100
    xpLoss :: Experience
    xpLoss = 70


beginPlay :: TVar Server.State -> Client -> TVar Room -> App ()
beginPlay state client roomVar = do
  liftIO $ Log.info $ printf "<%s>: Begin playing" (show $ Client.name client)
  added <- liftIO $  atomically $ addPlayerClient client roomVar
  case added of
    Nothing -> do
      liftIO $ Log.info $ printf "<%s>: Room is full" (show $ Client.name client)
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
  liftIO $ Log.info $ printf "<%s>: Begin spectating" (show $ Client.name client)
  finally
    (spectate client roomVar)
    (disconnect client roomVar state)

beginComputer :: Text -> TVar Server.State -> Client -> TVar Room -> App Bool
beginComputer cpuName state client roomVar = do
  liftIO $ Log.info $ printf "<%s>: Begin AI game" (show $ Client.name client)
  cpuGuid <- liftIO GUID.genText
  (computer, added) <- liftIO . atomically $ do
    computerAdded <- addComputerClient cpuName cpuGuid roomVar
    playerAdded <- addPlayerClient client roomVar
    return (computerAdded, playerAdded)
  case (,) <$> computer <*> added of
    Nothing -> do
      liftIO $ Log.info $ printf "<%s>: Room is full" (show $ Client.name client)
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
  liftIO $ Log.info $ printf "<%s>: Begin quickplay game" (show $ Client.name client)
  roomM <- liftIO . atomically $ Server.queue (client, roomVar) state
  case roomM of
    Just (_, existingRoom) -> do
      liftIO $ Log.info $ printf "<%s>: Joining existing quickplay room" (show $ Client.name client)
      beginPlay state client existingRoom
    Nothing -> do
      liftIO $ Log.info $ printf "<%s>: Creating new quickplay room" (show $ Client.name client)
      finally
        (beginPlay state client roomVar)
        (liftIO . atomically $ Server.dequeue client state)


beginWorld :: TVar Server.State -> Client -> Maybe World.WorldProgress -> App ()
beginWorld state client mProgress = do
  let username = Client.queryUsername client
  world <- World.getWorld username state mProgress
  Client.send (cs $ "world:" <> encode world) client
  msg <- Client.receive client
  let req = World.parseRequest msg
  case req of
    Just (World.JoinEncounter encounterId) ->
      case World.encounterFromGuid world encounterId of
        Just encounter -> do
          liftIO $ Log.info $ printf "<%s>: Joining world encounter" (show $ Client.name client)
          Client.send ("joinEncounter:" <> encounterId) client
          gen <- liftIO getGen
          let scenario = World.makeScenario encounter
          let roomName = encounterId
          let cpuName = World.encounter_name encounter
          roomVar <- liftIO . atomically $ Server.getOrCreateRoom roomName WaitCustom gen scenario state
          didWin <- beginComputer cpuName state client roomVar
          if didWin then
            (do
              let newProgress = World.WorldProgress $ World.encounter_key encounter
              liftIO $ Log.info $ printf "<%s>: Win! New world progress %s" (show $ Client.name client) (show newProgress)
              World.updateProgress username newProgress
              beginWorld state client (Just newProgress)
            )
            else
              (do
                liftIO $ Log.info $ printf "<%s>: Loss!" (show $ Client.name client)
                beginWorld state client mProgress
              )
        Nothing -> do
          liftIO $ Log.error $ printf "<%s>: No such encounter" (show $ Client.name client)
          Client.send "error:no such encounter" client
    Nothing -> do
      liftIO $ Log.error $ printf "<%s>: Unknown world request '%s'" (show $ Client.name client) msg
      Client.send "error:unknown world request" client


spectate :: Client -> TVar Room -> App ()
spectate client roomVar = do
  room <- liftIO . atomically $ addSpecClient client roomVar
  Client.send ("acceptSpec:" :: Text) client
  Room.broadcast (Command.toChat (SpectateCommand (Client.name client))) room
  syncClient client (Room.getState room)
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
  syncClient client (Room.getState room)
  forM_ outcomes (actOutcome room)
  _ <- runMaybeT . forever $ do
    msg <- lift $ Client.receive client
    let username = Client.name client
    let command = Command.parse username msg
    case command of
      EndEncounterCommand ->
        mzero -- Exit the loop, the encounter is over.
      _ ->
        lift $ actPlay command which roomVar username
  finalRoom <- liftIO $ readTVarIO roomVar
  return $ isWinner which $ Room.getState finalRoom


computerPlay :: WhichPlayer -> TVar Room -> App ()
computerPlay which roomVar = do
  _ <- runMaybeT . forever $ loop
  liftIO . Log.info $ printf "AI signing off"
  return ()
  where
    loop :: MaybeT App ()
    loop = do
      lift $ threadDelay 1000000
      gen <- liftIO $ getGen
      command <- lift $ chooseComputerCommand which roomVar gen
      case command of
        Just c -> do
          lift $ actPlay c which roomVar "CPU"
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
    Started (Playing m _) ->
      return $ trans <$> chooseAction gen which m (Room.getScenario r)
    _ ->
      return Nothing
  where
    trans :: Action -> Command
    trans EndAction          = EndTurnCommand
    trans (PlayAction index) = PlayCardCommand index
    randomChoice :: DeckBuilding.CharacterChoice
    randomChoice =
      DeckBuilding.CharacterChoice
        (DeckBuilding.character_name (randomCharacter genA))
        (DeckBuilding.rune_name (randomRune genB))
        (DeckBuilding.rune_name (randomRune genC))
        (DeckBuilding.rune_name (randomRune genD))
    (genA, genB) = split gen
    (genC, genD) = split genA
    randomCharacter :: Gen -> DeckBuilding.Character
    randomCharacter g = head $ shuffle g DeckBuilding.allCharacters
    randomRune :: Gen -> DeckBuilding.Rune
    randomRune g = head $ shuffle g DeckBuilding.allRunes


disconnect :: Client -> TVar Room -> TVar Server.State -> App (Server.State)
disconnect client roomVar state = do
  room <- liftIO . atomically $ Server.removeClient client roomVar
  Room.broadcast (Command.toChat . LeaveCommand $ Client.name client) room
  syncPlayersRoom room
  if Room.empty room then
    (liftIO $ do
      Log.info $ printf "<%s>: Room is empty, deleting room" (show $ Client.name client)
      atomically $ Server.deleteRoom (Room.getName room) state
    )
      else
        (liftIO $ do
          Log.info $ printf "<%s>: Room is not empty, retaining room" (show $ Client.name client)
          readTVarIO state
        )
