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
import GameState (EncodableOutcome(..), GameCommand(..), GameState(..), PlayState(..), Outcome(..), Username, reverso, update)
import Util (Err, Gen, getGen, modReturnTVar, shuffle)

import qualified Server
import Server (addComputerClient, addPlayerClient, addSpecClient)

import qualified Client
import Client (Client(..), ClientConnection(..))

import qualified Command
import Command (Command(..))

import qualified Room
import Room (Room)

import Data.Text (Text)
import qualified Data.Text.IO as T

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
  begin connection msg usernameM state
  WS.forkPingThread connection 30


begin :: WS.Connection -> Text -> Maybe Username -> TVar Server.State -> IO ()
begin conn roomReq loggedUsername state =
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
          roomVar <- atomically $ Server.getOrCreateRoom roomName gen state
          guid <- GUID.genText
          beginPrefix prefix state (client guid) roomVar
  where
    client :: Text -> Client
    client = Client (fromMaybe "Guest" loggedUsername) (PlayerConnection conn)


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


sendToPlayer :: WhichPlayer -> Text -> Room -> IO ()
sendToPlayer which msg room =
  case Room.getPlayerClient which room of
    Just client ->
      Client.send msg client
    Nothing ->
      return ()


sendToSpecs :: Text -> Room -> IO ()
sendToSpecs msg room =
  forM_ (Room.getSpecs room) (Client.send msg)


sendExcluding :: WhichPlayer -> Text -> Room -> IO ()
sendExcluding which msg room = do
  sendToSpecs msg room
  sendToPlayer (other which) msg room


spectate :: Client -> TVar Room -> IO ()
spectate client roomVar = do
  room <- atomically $ addSpecClient client roomVar
  Client.send ("acceptSpec:" :: Text) client
  broadcast (Command.toChat (SpectateCommand (Client.name client))) room
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


broadcast :: Text -> Room -> IO ()
broadcast msg room =
  forM_ (Room.getClients room) (Client.send msg)


disconnect :: Client -> TVar Room -> TVar Server.State -> IO (Server.State)
disconnect client roomVar state = do
  room <- atomically $ Server.removeClient client roomVar
  broadcast (Command.toChat . LeaveCommand $ Client.name client) room
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
          sendToPlayer which (Command.toChat (ErrorCommand err)) room
        Right outcomes ->
          forM_ outcomes $
            \outcome ->
              do
                logOutcome outcome
                actOutcome room outcome
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
  broadcast (Command.toChat cmd) room


actOutcome :: Room -> Outcome -> IO ()
actOutcome room SyncOutcome                = syncRoomClients room
actOutcome room (PlayCardOutcome which)    = sendExcluding which "playCard:" room
actOutcome room (EndTurnOutcome which)     = sendExcluding which "end:" room
actOutcome room (EncodableOutcome outcome) = actEncodable outcome
  where
    actEncodable :: EncodableOutcome -> IO ()
    actEncodable (HoverOutcome which _) =
      sendExcluding which (("hover:" <>) . cs . encode $ outcome) room
    actEncodable (ChatOutcome username msg) =
      broadcast ("chat:" <> username <> ": " <> msg) room
    actEncodable (ResolveOutcome models final) =
      resolveRoomClients (models, final) room


logOutcome :: Outcome -> IO ()
logOutcome SyncOutcome                = T.putStrLn "syncing"
logOutcome (PlayCardOutcome _)        = T.putStrLn "playing card"
logOutcome (EndTurnOutcome _)         = T.putStrLn "ending turn"
logOutcome (EncodableOutcome outcome) = logEncodable outcome
  where
    logEncodable :: EncodableOutcome -> IO()
    logEncodable (HoverOutcome _ _)   = T.putStrLn "hovering"
    logEncodable (ChatOutcome _ _)    = T.putStrLn "chatting"
    logEncodable (ResolveOutcome _ _) = T.putStrLn "resolving"


syncClient :: Client -> GameState -> IO ()
syncClient client game = Client.send (("sync:" <>) . cs . encode $ game) client


syncRoomClients :: Room -> IO ()
syncRoomClients room = do
  sendToPlayer PlayerA syncMsgPa room
  sendToPlayer PlayerB syncMsgPb room
  sendToSpecs syncMsgPa room
  where
    game = Room.getState room :: GameState
    syncMsgPa = ("sync:" <>) . cs . encode $ game :: Text
    syncMsgPb = ("sync:" <>) . cs . encode . reverso $ game :: Text


syncPlayersRoom :: Room -> IO ()
syncPlayersRoom room = do
  sendExcluding PlayerB (syncMsg True) room
  sendToPlayer PlayerB (syncMsg False) room
  where
    syncMsg :: Bool -> Text
    syncMsg rev =
      "syncPlayers:" <>
        (cs . encode . (if rev then reversoPlayers else id) $ Room.connected room)
    reversoPlayers (a, b) = (b, a)


resolveRoomClients :: ([Model], GameState) -> Room -> IO ()
resolveRoomClients (models, final) room = do
  sendToPlayer PlayerA resMsgPa room
  sendToPlayer PlayerB resMsgPb room
  sendToSpecs resMsgPa room
  where
    resMsgPa = ("res:" <>) . cs . encode $ outcome :: Text
    resMsgPb = ("res:" <>) . cs . encode $ reversoOutcome :: Text
    outcome = ResolveOutcome models final :: EncodableOutcome
    reversoOutcome = ResolveOutcome (modelReverso <$> models) (reverso final) :: EncodableOutcome
