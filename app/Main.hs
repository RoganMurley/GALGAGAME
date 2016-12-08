
module Main where

import Prelude hiding (lookup)

import Data.Aeson (encode)
import Data.Map.Strict (Map, delete, empty, insert, lookup)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar, readMVar)

import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Model (CardName, WhichPlayer(..))
import GameState (GameCommand(..), GameState(..), reverso, update)
import Room
import Util (getGen)


type ServerState = Map RoomName (MVar Room)

data Command =
    ChatCommand Username Text
  | PlayCommand Username
  | SpectateCommand Username
  | LeaveCommand Username
  | EndTurnCommand
  | PlayCardCommand CardName
  | HoverCardCommand CardName
  | RematchCommand
  | ErrorCommand Text


newServerState :: ServerState
newServerState = empty

getRoom :: RoomName -> MVar ServerState -> IO (MVar Room)
getRoom name state =
  modifyMVar state $ \s ->
    do
      case lookup name s of
        Just room ->
          return (s, room)
        Nothing ->
          do
            gen <- getGen
            r <- newMVar (newRoom gen)
            return (insert name r s, r)

deleteRoom :: RoomName -> MVar ServerState -> IO (ServerState)
deleteRoom name state =
  modifyMVar state $ \s ->
    let s' = delete name s in return (s', s')

stateUpdate :: GameCommand -> WhichPlayer -> MVar Room -> IO (Room)
stateUpdate cmd which room =
  modifyMVar room $ \r ->
    let r' = updateRoom r in return (r', r')
  where
    updateRoom (Room pa pb specs state) = Room pa pb specs (update cmd which state)

addSpecClient :: Client -> MVar Room -> IO (Room)
addSpecClient client room =
  modifyMVar room $ \r ->
    let r' = addSpec client r in return (r', r')

addPlayerClient :: Client -> MVar Room -> IO (Maybe (Room, WhichPlayer))
addPlayerClient client room =
  modifyMVar room $ \r ->
    case addPlayer client r of
      Just (r', p) ->
        return (r', Just (r', p))
      Nothing ->
        return (r, Nothing)

removeClient :: Client -> MVar Room -> IO (Room)
removeClient client room =
  modifyMVar room $ \r ->
    let r' = removeClientRoom client r in return (r', r')

broadcast :: Text -> Room -> IO ()
broadcast msg room = do
  -- T.putStrLn $ "Broadcasting: " <> msg
  forM_ (getRoomClients room) $ \(_, conn) -> WS.sendTextData conn msg

sendToPlayer :: WhichPlayer -> Text -> Room -> IO ()
sendToPlayer which msg room =
  case getPlayerClient which room of
    Just (_, conn) ->
      WS.sendTextData conn msg
    Nothing ->
      return ()

sendToSpecs :: Text -> Room -> IO ()
sendToSpecs msg room = do
  -- T.putStrLn ("To Specs: " <> msg)
  forM_ (getRoomSpecs room) $ \(_, conn) -> WS.sendTextData conn msg

-- MAIN STUFF.
main :: IO ()
main = do
   state <- newMVar newServerState
   WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  roomReq <- WS.receiveData conn
  case parseRoomReq roomReq of
    Just roomName -> do
      roomVar <- getRoom roomName state
      initialRoom <- readMVar roomVar
      msg <- WS.receiveData conn


      case parsePrefix msg of
        Nothing ->
          WS.sendTextData conn $ toChat $
            ErrorCommand $ "Connection protocol failure" <> msg

        Just prefix ->
          case prefix of
            _ | any ($ fst client) [ T.null ] ->
                WS.sendTextData conn $ toChat $
                  ErrorCommand "Name must be nonempty"

              | clientExists client initialRoom ->
                WS.sendTextData conn $ toChat $
                  ErrorCommand "User already exists"

              | prefix == "play:" ->
                do
                  added <- addPlayerClient client roomVar
                  case added of
                    Nothing ->
                      WS.sendTextData conn $ toChat $ ErrorCommand "Room is full :("
                    Just (room', which) ->
                      flip finally
                        (disconnect client roomVar roomName state)
                        (play which room' client roomVar)

              | prefix == "spectate:" ->
                flip finally
                  (disconnect client roomVar roomName state)
                  (spectate client roomVar)

              | otherwise ->
                WS.sendTextData conn $ toChat $
                  ErrorCommand "Something went terribly wrong in connection negotiation :("

              where
                client = (T.drop (T.length prefix) msg, conn) :: Client
    Nothing ->
      WS.sendTextData conn $ toChat $
        ErrorCommand "Bad room name protocol"

spectate :: Client -> MVar Room -> IO ()
spectate client@(user, conn) room = do
  room' <- addSpecClient client room
  WS.sendTextData conn ("acceptSpec:" :: Text)
  WS.sendTextData conn $ "chat:Welcome! " <> userList room'
  WS.sendTextData conn $ "chat:You're spectating " <> (getSpeccingName room')
  broadcast (toChat (SpectateCommand (fst client))) room'
  syncClient client (getRoomGameState room')
  forever $ do
    msg <- WS.receiveData conn
    actSpec (parseMsg user msg) room

play :: WhichPlayer -> Room -> Client -> MVar Room -> IO ()
play which room' client@(user, conn) room = do
  WS.sendTextData conn ("acceptPlay:" :: Text)
  WS.sendTextData conn ("chat:Welcome! " <> userList room')
  broadcast (toChat (PlayCommand (fst client))) room'
  syncRoomClients room'
  forever $ do
    msg <- WS.receiveData conn
    actPlay (parseMsg user msg) which room

disconnect :: Client -> MVar Room -> RoomName -> MVar ServerState -> IO (ServerState)
disconnect client room name state = do
  r <- removeClient client room
  broadcast (toChat (LeaveCommand (fst client))) r
  if roomEmpty r then
    deleteRoom name state
      else
        readMVar state

parsePrefix :: Text -> Maybe Text
parsePrefix msg
  | T.isPrefixOf "spectate:" msg = Just "spectate:"
  | T.isPrefixOf "play:" msg = Just "play:"
  | otherwise = Nothing

userList :: Room -> Text
userList room
  | users == "" = "You're the only one here..."
  | otherwise = "Users: " <> users
  where
    users :: Text
    users = T.intercalate ", " $ map fst $ getRoomClients room

actPlay :: Command -> WhichPlayer -> MVar Room -> IO ()
actPlay cmd which room =
  case trans cmd of
    Just command ->
      stateUpdate command which room >>= syncRoomClients
    Nothing ->
      actSpec cmd room
  where
    trans :: Command -> Maybe GameCommand
    trans EndTurnCommand = Just EndTurn
    trans (PlayCardCommand name) = Just (PlayCard name)
    trans (HoverCardCommand name) = Just (HoverCard name)
    trans RematchCommand = Just (Rematch)
    trans _ = Nothing

actSpec :: Command -> MVar Room -> IO ()
actSpec cmd room = readMVar room >>= broadcast (toChat cmd)

syncClient :: Client -> GameState -> IO ()
syncClient (_, conn) game =
  WS.sendTextData conn ("sync:" <> (cs $ encode $ game) :: Text)

syncRoomClients :: Room -> IO ()
syncRoomClients room = do
  sendToPlayer PlayerA syncMsgPa room
  sendToPlayer PlayerB syncMsgPb room
  sendToSpecs syncMsgPa room
  where
    game = getRoomGameState room :: GameState
    syncMsgPa = "sync:" <> (cs $ encode $ game) :: Text
    syncMsgPb = "sync:" <> (cs $ encode $ reverso $ game) :: Text


toChat :: Command -> Text
toChat (SpectateCommand name)     = "chat:" <> name <> " started spectating"
toChat (PlayCommand name)         = "chat:" <> name <> " started playing"
toChat (LeaveCommand name)        = "chat:" <> name <> " disconnected"
toChat (ChatCommand name message) = "chat:" <> name <> ": " <> message
toChat (ErrorCommand err)         = "error:" <> err
toChat _                          = "chat:" <> "Command cannot be processed to text :/"


parseMsg :: Username -> Text -> Command
parseMsg _ ""     = ErrorCommand "Command not found"
parseMsg name msg =
  case command of
    "end" ->
      EndTurnCommand
    "play" ->
      PlayCardCommand content
    "hover" ->
      HoverCardCommand content
    "chat" ->
      ChatCommand name content
    "rematch" ->
      RematchCommand
    _ ->
      ErrorCommand "Unknown Command"
  where
    parsed :: (Text, Text)
    parsed = T.breakOn ":" msg
    command = fst parsed :: Text
    content = T.drop 1 (snd parsed) :: Text

parseRoomReq :: Text -> Maybe Text
parseRoomReq msg =
  case parsed of
    ("room", name) ->
      Just name
    _ ->
      Nothing
  where
    parsed :: (Text, Text)
    parsed = T.breakOn ":" msg
