module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, readTVarIO)
import Control.Exception (finally)
import Control.Monad (forM_, forever, mzero, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)

import Network.Wai (Application)
import Network.Wai.Handler.WebSockets
import Network.Wai.Handler.Warp (run)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)

import ArtificalIntelligence (Action(..), chooseAction)
import Characters (CharModel(..), character_name, toList)
import Model (Model, modelReverso)
import Negotiation (Prefix(..), parseRoomReq, parsePrefix)
import Player (WhichPlayer(..), other)
import GameCommand (GameCommand(..), update)
import GameState (GameState(..), PlayState(..), reverso)
import Username (Username(Username))
import Util (Err, Gen, getGen, modReturnTVar, shuffle)

import qualified Server
import Server (addComputerClient, addPlayerClient, addSpecClient)

import qualified Client
import Client (Client(..), ClientConnection(..))

import qualified Outcome
import Outcome (Outcome)

import qualified Command
import Command (Command(..))

import qualified Room
import Room (Room)

import Data.Text (Text)

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


roomUpdate :: GameCommand -> WhichPlayer -> TVar Room -> STM (Room, Either Err [Outcome])
roomUpdate cmd which roomVar =
  modReturnTVar roomVar $ \room ->
    case updateRoom room of
      Left err ->
        (room, (room, Left err))
      Right (Nothing, o) ->
        (room, (room, Right o))
      Right (Just r, o) ->
        (r, (r, Right o))
  where
    updateRoom :: Room -> Either Err (Maybe Room, [Outcome])
    updateRoom room =
      case update cmd which (Room.getState room) of
        Left err ->
          Left err
        Right (newState, outcomes) ->
          Right ((Room.setState room) <$> newState, outcomes)


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
    trans EndAction = EndTurnCommand
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


actPlay :: Command -> WhichPlayer -> TVar Room -> IO ()
actPlay cmd which roomVar =
  case trans cmd of
    Just command -> do
      (room, updated) <- atomically $ roomUpdate command which roomVar
      case updated of
        Left err ->
          Room.sendToPlayer which (Command.toChat (ErrorCommand err)) room
        Right outcomes ->
          forM_ outcomes (actOutcome room)
    Nothing ->
      actSpec cmd roomVar
  where
    trans :: Command -> Maybe GameCommand
    trans EndTurnCommand             = Just EndTurn
    trans (PlayCardCommand index)    = Just (PlayCard index)
    trans (HoverCardCommand index)   = Just (HoverCard index)
    trans RematchCommand             = Just Rematch
    trans ConcedeCommand             = Just Concede
    trans (ChatCommand name content) = Just (Chat name content)
    trans (SelectCharacterCommand n) = Just (SelectCharacter n)
    trans _                          = Nothing


actSpec :: Command -> TVar Room -> IO ()
actSpec cmd roomVar = do
  room <- atomically $ readTVar roomVar
  Room.broadcast (Command.toChat cmd) room


syncClient :: Client -> GameState -> IO ()
syncClient client game = Client.send (("sync:" <>) . cs . encode $ game) client


syncRoomClients :: Room -> IO ()
syncRoomClients room = do
  Room.sendToPlayer PlayerA syncMsgPa room
  Room.sendToPlayer PlayerB syncMsgPb room
  Room.sendToSpecs syncMsgPa room
  where
    game = Room.getState room :: GameState
    syncMsgPa = ("sync:" <>) . cs . encode $ game :: Text
    syncMsgPb = ("sync:" <>) . cs . encode . reverso $ game :: Text


syncPlayersRoom :: Room -> IO ()
syncPlayersRoom room = do
  Room.sendExcluding PlayerB (syncMsg True) room
  Room.sendToPlayer PlayerB (syncMsg False) room
  where
    syncMsg :: Bool -> Text
    syncMsg rev =
      "syncPlayers:" <>
        (cs . encode . (if rev then reversoPlayers else id) $ Room.connected room)
    reversoPlayers (a, b) = (b, a)


resolveRoomClients :: ([Model], GameState) -> Room -> IO ()
resolveRoomClients (models, final) room = do
  Room.sendToPlayer PlayerA msgPa room
  Room.sendToPlayer PlayerB msgPb room
  Room.sendToSpecs msgPa room
  where
    msgPa = ("res:" <>) . cs . encode $ outcome :: Text
    msgPb = ("res:" <>) . cs . encode $ reversoOutcome :: Text
    outcome = Outcome.Resolve models final :: Outcome.Encodable
    reversoOutcome = Outcome.Resolve (modelReverso <$> models) (reverso final) :: Outcome.Encodable


actOutcome :: Room -> Outcome -> IO ()
actOutcome room Outcome.Sync =
  syncRoomClients room
actOutcome room (Outcome.PlayCard which) =
  Room.sendExcluding which "playCard:" room
actOutcome room (Outcome.EndTurn which) =
  Room.sendExcluding which "end:" room
actOutcome room (Outcome.Encodable o@(Outcome.Hover which _)) =
  Room.sendExcluding which (("hover:" <>) . cs . encode $ o) room
actOutcome room (Outcome.Encodable (Outcome.Chat (Username username) msg)) =
  Room.broadcast ("chat:" <> username <> ": " <> msg) room
actOutcome room (Outcome.Encodable (Outcome.Resolve models final)) =
  resolveRoomClients (models, final) room
