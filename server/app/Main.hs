module Main where

import Control.Concurrent (MVar, forkIO, newMVar, modifyMVar, readMVar, threadDelay)
import Control.Exception (finally)
import Control.Monad (forM_, forever, mzero, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (encode)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.HTTP.Types (status400)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.WebSockets
import Network.Wai.Handler.Warp (run)
import Safe (readMay)

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


main :: IO ()
main = do
  state <- newMVar Server.initState
  run 9160 $ waiApp state


waiApp :: MVar Server.State -> Application
waiApp state = websocketsOr WS.defaultConnectionOptions (wsApp state) backupApp
  where
    backupApp :: Application
    backupApp _ respond =
      respond $ responseLBS status400 [] "Not a WebSocket request"


wsApp :: MVar Server.State -> WS.ServerApp
wsApp state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  roomReq <- WS.receiveData conn
  case parseRoomReq roomReq of
    Just roomName -> do
      roomVar <- Server.getRoom roomName state
      initialRoom <- readMVar roomVar
      msg <- WS.receiveData conn

      case parsePrefix msg of
        Nothing ->
          (WS.sendTextData conn) . toChat $
            ErrorCommand $ "connection protocol failure" <> msg

        Just prefix ->
          case prefix of
            _ | T.null username ->
                (WS.sendTextData conn) . toChat $
                  ErrorCommand "name must be nonempty"

              | Room.clientExists client initialRoom ->
                (WS.sendTextData conn) . toChat $
                  ErrorCommand "username taken"

              | prefix == "play:" ->
                do
                  T.putStrLn "Custom game started"
                  added <- addPlayerClient client roomVar
                  case added of
                    Nothing ->
                      WS.sendTextData conn $ toChat $
                        ErrorCommand "room is full"
                    Just (room', which) ->
                      flip finally
                        (disconnect client roomVar roomName state)
                        (play which room' (username, conn) roomVar)

              | prefix == "spectate:" ->
                flip finally
                  (disconnect client roomVar roomName state)
                  (spectate (username, conn) roomVar)

              | prefix == "playComputer:" ->
                do
                  T.putStrLn "Computer game started"
                  computer <- addComputerClient roomVar
                  added <- addPlayerClient client roomVar
                  case (,) <$> computer <*> added of
                    Nothing ->
                      WS.sendTextData conn $ toChat $ ErrorCommand "room is full"
                    Just (computerClient, (room', which)) ->
                      flip finally
                        (do
                          _ <- disconnect client roomVar roomName state
                          disconnect computerClient roomVar roomName state
                        )
                        (do
                          _ <- forkIO (computerPlay (other which) roomVar)
                          play which room' (username, conn) roomVar
                        )

              | otherwise ->
                WS.sendTextData conn $ toChat $
                  ErrorCommand "something went wrong with connection negotiation"

              where
                username :: Username
                username = T.drop (T.length prefix) msg
                client :: Client
                client = Client username (PlayerConnection conn)
    Nothing ->
      (WS.sendTextData conn) . toChat $
        ErrorCommand ("bad room name protocol: " <> roomReq)


data Command =
    ChatCommand Username Text
  | PlayCommand Username
  | SpectateCommand Username
  | LeaveCommand Username
  | EndTurnCommand
  | PlayCardCommand Int
  | HoverCardCommand (Maybe Int)
  | RematchCommand
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

spectate :: (Username, WS.Connection) -> MVar Room -> IO ()
spectate (user, conn) room =
  let
    client = Client user (PlayerConnection conn) :: Client
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

play :: WhichPlayer -> Room -> (Username, WS.Connection) -> MVar Room -> IO ()
play which room' (user, conn) room =
  let
    client = Client user (PlayerConnection conn) :: Client
  in
    do
      Client.send ("acceptPlay:" :: Text) client
      Client.send ("chat:Welcome! " <> Room.userList room') client
      broadcast (toChat (PlayCommand (Client.name client))) room'
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
        $ shuffle allChars gen


broadcast :: Text -> Room -> IO ()
broadcast msg room =
  forM_ (Room.getClients room) (Client.send msg)


disconnect :: Client -> MVar Room -> Room.Name -> MVar Server.State -> IO (Server.State)
disconnect client room name state = do
  r <- Server.removeClient client room
  broadcast (toChat (LeaveCommand (Client.name client))) r
  Server.deleteRoom name state


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
