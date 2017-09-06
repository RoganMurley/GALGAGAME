module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, readTVarIO)
import Control.Exception (finally)
import Control.Monad (forever, mzero, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)

import Network.Wai (Application)
import Network.Wai.Handler.WebSockets
import Network.Wai.Handler.Warp (run)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)

import Act (actPlay, actSpec, syncClient, syncPlayersRoom, syncRoomClients)
import ArtificalIntelligence (Action(..), chooseAction)
import Characters (CharModel(..), character_name, toList)
import Negotiation (Prefix(..), parseRoomReq, parsePrefix)
import Player (WhichPlayer(..), other)
import GameState (GameState(..), PlayState(..))
import Username (Username(Username))
import Util (Gen, getGen, shuffle)

import qualified Server
import Server (addComputerClient, addPlayerClient, addSpecClient)

import qualified Client
import Client (Client(..), ClientConnection(..))

import qualified Command
import Command (Command(..))

import qualified Room
import Room (Room)

import qualified Network.WebSockets as WS

import qualified Auth as A
import qualified Database.Redis as R
import qualified Data.GUID as GUID


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  userConn  <- R.connect $ A.connectInfo A.UserDatabase
  tokenConn <- R.connect $ A.connectInfo A.TokenDatabase
  authApp   <- A.app userConn tokenConn
  state     <- atomically $ newTVar Server.initState
  run 9160 $ waiApp state tokenConn authApp


waiApp :: TVar Server.State -> R.Connection -> Application -> Application
waiApp state tokenConn backupApp =
  websocketsOr WS.defaultConnectionOptions (wsApp state tokenConn) backupApp


wsApp :: TVar Server.State -> R.Connection -> WS.ServerApp
wsApp state tokenConn pending = do
  connection <- WS.acceptRequest pending
  msg        <- WS.receiveData connection
  usernameM  <- A.checkAuth tokenConn $ A.getToken pending
  WS.forkPingThread connection 30
  begin connection msg (Username <$> usernameM) state


begin :: WS.Connection -> Text -> Maybe Username -> TVar Server.State -> IO ()
begin conn roomReq usernameM state =
  case parseRoomReq roomReq of
    Nothing ->
      (WS.sendTextData conn) . Command.toChat . ErrorCommand $ "bad room name protocol: " <> roomReq
    Just roomName -> do
      msg <- WS.receiveData conn
      case parsePrefix msg of
        Nothing ->
          (WS.sendTextData conn) . Command.toChat . ErrorCommand $ "connection protocol failure" <> msg
        Just prefix -> do
          gen <- getGen
          guid <- GUID.genText
          roomVar <- atomically $ Server.getOrCreateRoom roomName gen state
          beginPrefix
            prefix
            state
            (Client (fromMaybe (Username "Guest") usernameM) (PlayerConnection conn) guid)
            roomVar


beginPrefix :: Prefix -> TVar Server.State -> Client -> TVar Room -> IO ()
beginPrefix PrefixPlay = beginPlay
beginPrefix PrefixSpec = beginSpec
beginPrefix PrefixCpu  = beginComputer


beginPlay :: TVar Server.State -> Client -> TVar Room -> IO ()
beginPlay state client roomVar = do
  added <- atomically $ addPlayerClient client roomVar
  case added of
    Nothing ->
      Client.send (Command.toChat $ ErrorCommand "room is full") client
    Just which ->
      finally
        (play which client roomVar)
        (disconnect client roomVar state)


beginSpec :: TVar Server.State -> Client -> TVar Room -> IO ()
beginSpec state client roomVar =
  finally
    (spectate client roomVar)
    (disconnect client roomVar state)


beginComputer :: TVar Server.State -> Client -> TVar Room -> IO ()
beginComputer state client roomVar = do
  cpuGuid  <- GUID.genText
  (computer, added) <- atomically $ do
    computerAdded <- addComputerClient cpuGuid roomVar
    playerAdded <- addPlayerClient client roomVar
    return (computerAdded, playerAdded)
  case (,) <$> computer <*> added of
    Nothing ->
      Client.send (Command.toChat $ ErrorCommand "room is full") client
    Just (computerClient, which) ->
      finally
        (do
          _ <- forkIO (computerPlay (other which) roomVar)
          (play which client roomVar))
        (do
          _ <- disconnect computerClient roomVar state
          (disconnect client roomVar state))


spectate :: Client -> TVar Room -> IO ()
spectate client roomVar = do
  room <- atomically $ addSpecClient client roomVar
  Client.send ("acceptSpec:" :: Text) client
  Room.broadcast (Command.toChat (SpectateCommand (Client.name client))) room
  syncClient client (Room.getState room)
  forever $ do
    msg <- Client.receive client
    actSpec (Command.parse (Client.name client) msg) roomVar


play :: WhichPlayer -> Client -> TVar Room -> IO ()
play which client roomVar = do
  Client.send ("acceptPlay:" :: Text) client
  room <- atomically $ readTVar roomVar
  syncPlayersRoom room
  syncRoomClients room
  forever $ do
    msg <- Client.receive client
    actPlay (Command.parse (Client.name client) msg) which roomVar


computerPlay :: WhichPlayer -> TVar Room -> IO ()
computerPlay which roomVar =
  do
  _ <- runMaybeT . forever $ do
    lift $ threadDelay 1000000
    command <- lift $ chooseComputerCommand which roomVar
    case command of
      Just c -> do
        lift $ actPlay c which roomVar
        lift $ threadDelay 10000
      Nothing ->
        return ()
    -- Break out if the room's empty.
    room <- lift . atomically $ readTVar roomVar
    when (Room.empty room) mzero
  return ()


chooseComputerCommand :: WhichPlayer -> TVar Room -> IO (Maybe Command)
chooseComputerCommand which room = do
  r <- atomically $ readTVar room
  case Room.getState r of
    Selecting charModel _ gen ->
      return . Just . SelectCharacterCommand $ randomChar charModel gen
    Started (Playing m) ->
      return $ trans <$> chooseAction which m
    _ ->
      return Nothing
  where
    trans :: Action -> Command
    trans EndAction          = EndTurnCommand
    trans (PlayAction index) = PlayCardCommand index
    randomChar :: CharModel -> Gen -> Text
    randomChar (CharModel selected _ allChars) gen =
      character_name . head . (drop (length . Characters.toList $ selected))
        $ shuffle gen allChars


disconnect :: Client -> TVar Room -> TVar Server.State -> IO (Server.State)
disconnect client roomVar state = do
  room <- atomically $ Server.removeClient client roomVar
  Room.broadcast (Command.toChat . LeaveCommand $ Client.name client) room
  syncPlayersRoom room
  if Room.empty room then
    atomically $ Server.deleteRoom (Room.getName room) state
      else
        readTVarIO state
