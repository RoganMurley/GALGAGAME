module Main where

import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Monad (forM_)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, readTVarIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Exception.Lifted (finally)
import Control.Monad (forever, mzero, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Text.Printf (printf)

import Network.Wai (Application)
import Network.Wai.Handler.WebSockets
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import System.Log.Logger (Priority(DEBUG), infoM, warningM, setLevel, updateGlobalLogger)

import Act (actOutcome, actPlay, actSpec, syncClient, syncPlayersRoom, syncClient)
import ArtificalIntelligence (Action(..), chooseAction)
import Config (App, ConnectInfoConfig(..), runApp)
import Database (postgresConnectInfo, redisConnectInfo)
import GameState (GameState(..), PlayState(..), WaitType(..))
import Model (Turn)
import Negotiation (Prefix(..), RoomRequest(..), parseRoomReq, parsePrefix)
import Outcome (Outcome)
import Player (WhichPlayer(..), other)
import Scenario (Scenario(..))
import Start (startProgram, tutorialStartProgram)
import Stats.Stats (Experience)
import User (User(..), getUsername, getUserFromToken)
import Util (Gen, getGen, shuffle)

import qualified DSL.Beta as Beta

import qualified Server
import Server (addComputerClient, addPlayerClient, addSpecClient)

import qualified Characters
import Characters (CharModel(..), allSelected, character_name, toList)

import qualified Client
import Client (Client(..), ClientConnection(..))

import qualified Command
import Command (Command(..))

import qualified Room
import Room (Room)

import qualified Replay.Final

import qualified Network.WebSockets as WS

import qualified Auth.Apps as Auth
import qualified Auth.Views as Auth
import qualified Data.GUID as GUID


main :: IO ()
main = do
  updateGlobalLogger "app" $ setLevel DEBUG
  hSetBuffering stdout LineBuffering

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
    warningM "app" str
    WS.sendTextData conn . Command.toChat . ErrorCommand $ cs str


begin :: WS.Connection -> Text -> User -> TVar Server.State -> App ()
begin conn roomReq user state = do
  let username = getUsername user :: Text
  liftIO $ infoM "app" $ printf "<%s>: New connection" username
  case parseRoomReq roomReq of
    Just (RoomRequest roomName) -> do
      liftIO $ infoM "app" $ printf "<%s>: Requesting room [%s]" username roomName
      msg <- liftIO $ WS.receiveData conn
      case parsePrefix msg of
        Nothing ->
          connectionFail conn $ printf "<%s>: Connection protocol failure" msg
        Just prefix -> do
          liftIO $ infoM "app" $ printf "<%s>: %s" username (show prefix)
          gen <- liftIO getGen
          guid <- liftIO GUID.genText
          let scenario = makeScenario prefix
          roomVar <- liftIO . atomically $ Server.getOrCreateRoom roomName (prefixWaitType prefix) gen scenario state
          beginPrefix
            prefix
            state
            (Client user (PlayerConnection conn) guid)
            roomVar
    Just (PlayReplayRequest replayId) -> do
      mReplay <- Replay.Final.load replayId
      case mReplay of
        Just replay ->
          liftIO $ WS.sendTextData conn ("replay:" <> replay)
        Nothing ->
          liftIO $ WS.sendTextData conn ("replayNotFound:" :: Text)
    Nothing ->
      connectionFail conn $ printf "<%s>: Bad room name protocol %s" (show username) roomReq


beginPrefix :: Prefix -> TVar Server.State -> Client -> TVar Room -> App ()
beginPrefix PrefixPlay     = beginPlay
beginPrefix PrefixSpec     = beginSpec
beginPrefix PrefixCpu      = beginComputer
beginPrefix PrefixTutorial = beginComputer
beginPrefix PrefixDaily    = beginComputer
beginPrefix PrefixQueue    = beginQueue


prefixWaitType :: Prefix -> WaitType
prefixWaitType PrefixQueue = WaitQuickplay
prefixWaitType _           = WaitCustom


makeScenario :: Prefix -> Scenario
makeScenario prefix =
  Scenario {
    scenario_turn = turn
  , scenario_charactersPa = characters
  , scenario_charactersPb = characters
  , scenario_prog = prog
  , scenario_xpWin = xpWin
  , scenario_xpLoss = xpLoss
  }
  where
    characters :: Maybe Characters.FinalSelection
    characters =
      case prefix of
        PrefixTutorial ->
          Just (Characters.breaker, Characters.shielder, Characters.striker)
        PrefixDaily ->
          Just (Characters.shielder, Characters.balancer, Characters.balancer)
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
    xpWin =
      case prefix of
        PrefixDaily ->
          200
        _ ->
          100
    xpLoss :: Experience
    xpLoss = 70


beginPlay :: TVar Server.State -> Client -> TVar Room -> App ()
beginPlay state client roomVar = do
  liftIO $ infoM "app" $ printf "<%s>: Begin playing" (show $ Client.name client)
  added <- liftIO $  atomically $ addPlayerClient client roomVar
  case added of
    Nothing -> do
      liftIO $ infoM "app" $ printf "<%s>: Room is full" (show $ Client.name client)
      Client.send (Command.toChat $ ErrorCommand "room is full") client
    Just (which, outcomes) ->
      finally
        (play which client roomVar outcomes)
        (disconnect client roomVar state)


beginSpec :: TVar Server.State -> Client -> TVar Room -> App ()
beginSpec state client roomVar = do
  liftIO $ infoM "app" $ printf "<%s>: Begin spectating" (show $ Client.name client)
  finally
    (spectate client roomVar)
    (disconnect client roomVar state)

beginComputer :: TVar Server.State -> Client -> TVar Room -> App ()
beginComputer state client roomVar = do
  liftIO $ infoM "app" $ printf "<%s>: Begin AI game" (show $ Client.name client)
  cpuGuid <- liftIO GUID.genText
  (computer, added) <- liftIO . atomically $ do
    computerAdded <- addComputerClient cpuGuid roomVar
    playerAdded <- addPlayerClient client roomVar
    return (computerAdded, playerAdded)
  case (,) <$> computer <*> added of
    Nothing -> do
      liftIO $ infoM "app" $ printf "<%s>: Room is full" (show $ Client.name client)
      Client.send (Command.toChat $ ErrorCommand "Room is full") client
    Just (computerClient, (which, outcomes)) ->
      finally
        (do
          _ <- fork (computerPlay (other which) roomVar)
          (play which client roomVar outcomes))
        (do
          _ <- disconnect computerClient roomVar state
          (disconnect client roomVar state))


beginQueue :: TVar Server.State -> Client -> TVar Room -> App ()
beginQueue state client roomVar = do
  liftIO $ infoM "app" $ printf "<%s>: Begin quickplay game" (show $ Client.name client)
  roomM <- liftIO . atomically $ Server.queue (client, roomVar) state
  case roomM of
    Just (_, existingRoom) -> do
      liftIO $ infoM "app" $ printf "<%s>: Joining existing quickplay room" (show $ Client.name client)
      beginPlay state client existingRoom
    Nothing -> do
      liftIO $ infoM "app" $ printf "<%s>: Creating new quickplay room" (show $ Client.name client)
      finally
        (beginPlay state client roomVar)
        (liftIO . atomically $ Server.dequeue client state)


spectate :: Client -> TVar Room -> App ()
spectate client roomVar = do
  room <- liftIO . atomically $ addSpecClient client roomVar
  Client.send ("acceptSpec:" :: Text) client
  Room.broadcast (Command.toChat (SpectateCommand (Client.name client))) room
  syncClient client (Room.getState room)
  _ <- runMaybeT . forever $ do
    msg <- lift $ Client.receive client
    lift $ actSpec (Command.parse (Client.name client) msg) roomVar
  return ()


play :: WhichPlayer -> Client -> TVar Room -> [Outcome] -> App ()
play which client roomVar outcomes = do
  Client.send ("acceptPlay:" :: Text) client
  room <- liftIO . atomically $ readTVar roomVar
  syncPlayersRoom room
  syncClient client (Room.getState room)
  forM_ outcomes (actOutcome room)
  _ <- runMaybeT . forever $ do
    msg <- lift $ Client.receive client
    lift $ actPlay (Command.parse (Client.name client) msg) which roomVar username
  return ()
  where
    username = Client.name client


computerPlay :: WhichPlayer -> TVar Room -> App ()
computerPlay which roomVar = do
  _ <- runMaybeT . forever $ loop
  liftIO . infoM "app" $ printf "AI signing off"
  return ()
  where
    loop :: MaybeT App ()
    loop =
      do
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
        room <- liftIO . atomically $ readTVar roomVar
        when (Room.empty room) mzero


chooseComputerCommand :: WhichPlayer -> TVar Room -> Gen -> App (Maybe Command)
chooseComputerCommand which room gen = do
  r <- liftIO . atomically $ readTVar room
  case Room.getState r of
    Selecting charModel _ _ ->
      if allSelected charModel which then
        return Nothing
          else
            return . Just . SelectCharacterCommand $ randomChar charModel
    Started (Playing m _) ->
      return $ trans <$> chooseAction gen which m (Room.getScenario r)
    _ ->
      return Nothing
  where
    trans :: Action -> Command
    trans EndAction          = EndTurnCommand
    trans (PlayAction index) = PlayCardCommand index
    randomChar :: CharModel -> Text
    randomChar (CharModel selected _ allChars) =
      character_name . head . (drop (length . Characters.toList $ selected))
        $ shuffle gen allChars


disconnect :: Client -> TVar Room -> TVar Server.State -> App (Server.State)
disconnect client roomVar state = do
  room <- liftIO . atomically $ Server.removeClient client roomVar
  Room.broadcast (Command.toChat . LeaveCommand $ Client.name client) room
  syncPlayersRoom room
  if Room.empty room then
    (liftIO $ do
      infoM "app" $ printf "<%s>: Room is empty, deleting room" (show $ Client.name client)
      atomically $ Server.deleteRoom (Room.getName room) state
    )
      else
        (liftIO $ do
          infoM "app" $ printf "<%s>: Room is not empty, retaining room" (show $ Client.name client)
          readTVarIO state
        )
