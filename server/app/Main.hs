module Main where

import Control.Concurrent (MVar, forkIO, newMVar, modifyMVar, readMVar, threadDelay)
import Control.Exception (finally)
import Control.Monad (forM_, forever, mzero, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets
import Network.Wai.Handler.Warp (run)
import Safe (readMay)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import Web.Cookie (parseCookiesText)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import ArtificalIntelligence (Action(..), chooseAction)
import Characters (CharModel(..), character_name, toList)
import Model (Model, modelReverso)
import Player (WhichPlayer(..), other)
import GameState (EncodableOutcome(..), GameCommand(..), GameState(..), PlayState(..), Outcome(..), Username, reverso, update)
import Util (Err, Gen, shuffle)

import qualified Server
import Server (addComputerClient, addPlayerClient, addSpecClient)

import qualified Client
import Client (Client(..), ClientConnection(..))

import qualified Room
import Room (Room)

import qualified Auth as A
import qualified Database.Redis as R
import qualified Data.GUID as GUID


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  state <- newMVar Server.initState
  userConn  <- R.connect $ A.connectInfo A.UserDatabase
  tokenConn <- R.connect $ A.connectInfo A.TokenDatabase
  authApp <- A.app userConn tokenConn
  run 9160 $ waiApp state tokenConn authApp


waiApp :: MVar Server.State -> R.Connection -> Application -> Application
waiApp state tokenConn backupApp =
  websocketsOr WS.defaultConnectionOptions (wsApp state tokenConn) backupApp


wsApp :: MVar Server.State -> R.Connection -> WS.ServerApp
wsApp state tokenConn pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  roomReq <- WS.receiveData conn

  case getToken pending of
    Nothing ->
      begin conn roomReq Nothing state
    Just token -> do
      storedToken <- A.checkAuth tokenConn token
      begin conn roomReq storedToken state


begin :: WS.Connection -> Text -> Maybe Username -> MVar Server.State -> IO ()
begin conn roomReq loggedUsername state =
  case parseRoomReq roomReq of
    Just roomName -> do
      roomVar <- Server.getRoom roomName state
      msg <- WS.receiveData conn
      guid <- GUID.genText

      case parsePrefix msg of
        Nothing ->
          (WS.sendTextData conn) . toChat $
            ErrorCommand $ "connection protocol failure" <> msg

        Just prefix ->
          case prefix of
            _ | prefix == "play:" ->
                do
                  T.putStrLn "Custom game started"
                  added <- addPlayerClient client roomVar
                  case added of
                    Nothing ->
                      WS.sendTextData conn $ toChat $
                        ErrorCommand "room is full"
                    Just (room', which) ->
                      flip finally
                        (disconnect client (Just which) roomVar roomName state)
                        (play which room' (username, conn, guid) roomVar)

              | prefix == "spectate:" ->
                flip finally
                  (disconnect client Nothing roomVar roomName state)
                  (spectate (username, conn, guid) roomVar)

              | prefix == "playComputer:" ->
                do
                  T.putStrLn "Computer game started"
                  cpuGuid <- GUID.genText
                  computer <- addComputerClient cpuGuid roomVar
                  added <- addPlayerClient client roomVar
                  case (,) <$> computer <*> added of
                    Nothing ->
                      WS.sendTextData conn $ toChat $ ErrorCommand "room is full"
                    Just (computerClient, (room', which)) ->
                      flip finally
                        (do
                          _ <- disconnect client (Just which) roomVar roomName state
                          disconnect computerClient (Just . other $ which) roomVar roomName state
                        )
                        (do
                          _ <- forkIO (computerPlay (other which) roomVar)
                          play which room' (username, conn, guid) roomVar
                        )

              | otherwise ->
                WS.sendTextData conn $ toChat $
                  ErrorCommand "something went wrong with connection negotiation"

              where
                username :: Username
                username = fromMaybe "Guest" loggedUsername
                client :: Client
                client = Client username (PlayerConnection conn) guid
    Nothing ->
      (WS.sendTextData conn) . toChat $
        ErrorCommand ("bad room name protocol: " <> roomReq)


getToken :: WS.PendingConnection -> Maybe A.Token
getToken pending = loginCookie
  where
    loginCookie :: Maybe A.Token
    loginCookie = snd <$> find (\x -> fst x == "login") cookies
    cookies :: [(Text, Text)]
    cookies = case cookieString of
      Just str ->
        parseCookiesText str
      Nothing ->
        []
    cookieString :: Maybe ByteString
    cookieString = snd <$> find (\x -> fst x == "Cookie") headers
    headers :: WS.Headers
    headers = WS.requestHeaders . WS.pendingRequest $ pending


data Command =
    ChatCommand Username Text
  | PlayCommand Username
  | SpectateCommand Username
  | LeaveCommand Username
  | EndTurnCommand
  | PlayCardCommand Int
  | HoverCardCommand (Maybe Int)
  | RematchCommand
  | ConcedeCommand
  | SelectCharacterCommand Text
  | ErrorCommand Text
  deriving (Show)


roomUpdate :: GameCommand -> WhichPlayer -> MVar Room -> IO (Either Err (Room, [Outcome]))
roomUpdate cmd which room =
  modifyMVar room $ \r ->
    case updateRoom r of
      Left err ->
        return (r, Left err)
      Right (Nothing, o) ->
        return (r, Right (r, o))
      Right (Just r', o) ->
        return (r', Right (r', o))
  where
    updateRoom :: Room -> Either Err (Maybe Room, [Outcome])
    updateRoom r =
      case update cmd which (Room.getState r) of
        Left err ->
          Left err
        Right (newState, outcomes) ->
          Right ((Room.setState r) <$> newState, outcomes)

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

spectate :: (Username, WS.Connection, Text) -> MVar Room -> IO ()
spectate (user, conn, guid) room =
  let
    client = Client user (PlayerConnection conn) guid :: Client
  in
    do
      room' <- addSpecClient client room
      Client.send ("acceptSpec:" :: Text) client
      Client.send ("chat:Welcome! " <> Room.userList room') client
      Client.send ("chat:You're spectating " <> (Room.getSpeccingName room')) client
      broadcast (toChat (SpectateCommand (Client.name client))) room'
      syncClient client (Room.getState room')
      forever $ do
        msg <- WS.receiveData conn
        actSpec (parseMsg user msg) room

play :: WhichPlayer -> Room -> (Username, WS.Connection, Text) -> MVar Room -> IO ()
play which room' (user, conn, guid) room =
  let
    client = Client user (PlayerConnection conn) guid :: Client
  in
    do
      Client.send ("acceptPlay:" :: Text) client
      Client.send ("chat:Welcome! " <> Room.userList room') client
      broadcast (toChat (PlayCommand (Client.name client))) room'
      syncPlayersRoom room'
      syncRoomClients room'
      forever $ do
        msg <- WS.receiveData conn
        actPlay (parseMsg user msg) which room


-- Switch from transformers to mtl?
computerPlay :: WhichPlayer -> MVar Room -> IO ()
computerPlay which room =
  do
  _ <- runMaybeT $ forever $ do
    lift $ threadDelay 1000000
    command <- lift $ chooseComputerCommand which room
    case command of
      Just c ->
        lift $ actPlay c which room
      Nothing ->
        return ()

    -- Break out if the room's empty.
    r <- lift $ readMVar room
    when (Room.empty r) mzero
  return ()


chooseComputerCommand :: WhichPlayer -> MVar Room -> IO (Maybe Command)
chooseComputerCommand which room = do
  r <- readMVar room
  case Room.getState r of
    Selecting charModel _ gen ->
      return . Just . SelectCharacterCommand $ randomChar charModel gen
    Started (Playing m) ->
      return . (fmap trans) $ chooseAction which m
    _ ->
      return Nothing
  where
    -- So ugly kill me
    trans :: Action -> Command
    trans EndAction = EndTurnCommand
    trans (PlayAction index) = PlayCardCommand index
    -- Unsafe and ugly fix me please
    randomChar :: CharModel -> Gen -> Text
    randomChar (CharModel selected _ allChars) gen =
      character_name . head . (drop (length . Characters.toList $ selected))
        $ shuffle gen allChars


broadcast :: Text -> Room -> IO ()
broadcast msg room =
  forM_ (Room.getClients room) (Client.send msg)


disconnect :: Client -> Maybe WhichPlayer -> MVar Room -> Room.Name -> MVar Server.State -> IO (Server.State)
disconnect client mWhich room name state = do
  r <- Server.removeClient client room
  broadcast (toChat (LeaveCommand (Client.name client))) r
  case mWhich of -- Remove this?
    Nothing -> return ()
    Just _ ->  syncPlayersRoom r
  if Room.empty r then
    Server.deleteRoom name state
      else
        readMVar state

actPlay :: Command -> WhichPlayer -> MVar Room -> IO ()
actPlay cmd which roomVar =
  case trans cmd of
    Just command -> do
      updated <- roomUpdate command which roomVar
      case updated of
        Left err -> do
          room <- readMVar roomVar
          sendToPlayer which (toChat (ErrorCommand err)) room
        Right (room, outcomes) ->
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


actSpec :: Command -> MVar Room -> IO ()
actSpec cmd room =
  readMVar room >>= broadcast (toChat cmd)


actOutcome :: Room -> Outcome -> IO ()
actOutcome room SyncOutcome =
  syncRoomClients room
actOutcome room (PlayCardOutcome which) =
  sendExcluding which "playCard:" room
actOutcome room (EndTurnOutcome which) =
  sendExcluding which "end:" room
actOutcome room (EncodableOutcome outcome) =
  actEncodable outcome
    where
      actEncodable :: EncodableOutcome -> IO ()
      actEncodable (HoverOutcome which _) =
        sendExcluding which (("hover:" <>) . cs . encode $ outcome) room
      actEncodable (ChatOutcome username msg) =
        broadcast ("chat:" <> username <> ": " <> msg) room
      actEncodable (ResolveOutcome models final) =
        resolveRoomClients (models, final) room


logOutcome :: Outcome -> IO ()
logOutcome SyncOutcome =
  T.putStrLn "syncing"
logOutcome (PlayCardOutcome _) =
  T.putStrLn "playing card"
logOutcome (EndTurnOutcome _) =
  T.putStrLn "ending turn"
logOutcome (EncodableOutcome outcome) =
  logEncodable outcome
    where
      logEncodable :: EncodableOutcome -> IO()
      logEncodable (HoverOutcome _ _) =
        T.putStrLn "hovering"
      logEncodable (ChatOutcome _ _) =
        T.putStrLn "chatting"
      logEncodable (ResolveOutcome _ _) =
        T.putStrLn "resolving"


syncClient :: Client -> GameState -> IO ()
syncClient client game =
  Client.send (("sync:" <>) . cs . encode $ game) client


syncRoomClients :: Room -> IO ()
syncRoomClients room = do
  sendToPlayer PlayerA syncMsgPa room
  sendToPlayer PlayerB syncMsgPb room
  sendToSpecs syncMsgPa room
  where
    game = Room.getState room :: GameState
    syncMsgPa = ("sync:" <>) . cs . encode $ game :: Text
    syncMsgPb = ("sync:" <>) . cs . encode . reverso $ game :: Text


-- TIDY ME
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


toChat :: Command -> Text
toChat (SpectateCommand name) =
  "chat:" <> name <> " started spectating"
toChat (PlayCommand name) =
  "chat:" <> name <> " started playing"
toChat (LeaveCommand name) =
  "chat:" <> name <> " disconnected"
toChat (ChatCommand name message) =
  "chat:" <> name <> ": " <> message
toChat (ErrorCommand err) =
  "error:" <> err
toChat _ =
  "chat:" <> "Command cannot be processed to text :/"


-- PARSING.
parseMsg :: Username -> Text -> Command
parseMsg _ ""     = ErrorCommand "Command not found"
parseMsg name msg =
  case command of
    "end" ->
      EndTurnCommand
    "play" ->
      case readMay . cs $ content of
        Just index ->
          PlayCardCommand index
        Nothing ->
          ErrorCommand (content <> " not a hand card index")
    "hover" ->
      case content of
        "null" ->
          HoverCardCommand Nothing
        _ ->
          case readMay . cs $ content of
            Just index ->
              HoverCardCommand (Just index)
            Nothing ->
              ErrorCommand (content <> " not a hand card index")
    "chat" ->
      ChatCommand name content
    "rematch" ->
      RematchCommand
    "concede" ->
      ConcedeCommand
    "selectCharacter" ->
      SelectCharacterCommand content
    _ ->
      ErrorCommand ("Unknown Command " <> (cs (show command)))
  where
    parsed = T.breakOn ":" msg :: (Text, Text)
    command = fst parsed :: Text
    content = T.drop 1 (snd parsed) :: Text


parseRoomReq :: Text -> Maybe Text
parseRoomReq msg =
  case T.breakOn ":" msg of
    ("room", name) ->
      Just . (T.drop 1) $ name
    _ ->
      Nothing


parsePrefix :: Text -> Maybe Text
parsePrefix msg
  | T.isPrefixOf "spectate:"     msg = Just "spectate:"
  | T.isPrefixOf "play:"         msg = Just "play:"
  | T.isPrefixOf "playComputer:" msg = Just "playComputer:"
  | otherwise                        = Nothing
