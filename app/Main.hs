{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (lookup)

import Data.Aeson (encode)
import Data.Char (isPunctuation, isSpace)
import Data.Map.Strict (Map, delete, empty, insert, lookup)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, maybeToList)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, putMVar, readMVar, takeMVar)
import System.Random (StdGen, getStdGen)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import GameState (CardName, GameCommand(..), Model(..), WhichPlayer(..), reverso, update)
import Room


type ServerState = Map RoomName (MVar Room)

data Command =
    ChatCommand Username Text
  | PlayCommand Username
  | SpectateCommand Username
  | LeaveCommand Username
  | DrawCommand
  | EndTurnCommand
  | PlayCardCommand CardName
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
            gen <- getStdGen
            r <- newMVar (newRoom gen)
            return (insert name r s, r)

deleteRoom :: RoomName -> MVar ServerState -> IO (ServerState)
deleteRoom name state =
  modifyMVar state $ \s ->
    let s' = delete name s in return (s', s')

stateUpdate :: GameCommand -> WhichPlayer -> MVar Room -> IO (Room)
stateUpdate cmd which room =
  modifyMVar room $ \r -> return (gameUpdate cmd r, gameUpdate cmd r)
  where
    gameUpdate :: GameCommand -> Room -> Room
    gameUpdate cmd (Room pa pb specs model) = Room pa pb specs (fromMaybe model (update cmd which model)) -- LOOKS DANGEROUS?

addSpecClient :: Client -> MVar Room -> IO (Room)
addSpecClient client room =
  modifyMVar room $ \r ->
    let r' = addSpec client r in return (r', r')

addPlayerClient :: Client -> MVar Room -> IO (Room, WhichPlayer)
addPlayerClient client room =
  modifyMVar room $ \r ->
    let r' = addPlayer client r in return (fst r', r')

removeClient :: Client -> MVar Room -> IO (Room)
removeClient client room =
  modifyMVar room $ \r ->
    let r' = removeClientRoom client r in return (r', r')

broadcast :: Text -> Room -> IO ()
broadcast msg room = do
  T.putStrLn $ "Broadcasting: " <> msg
  forM_ (getRoomClients room) $ \(_, conn) -> WS.sendTextData conn msg

sendToPlayer :: WhichPlayer -> Text -> Room -> IO ()
sendToPlayer which msg room =
  case getPlayerClient which room of
    Just (_, conn) -> do
      T.putStrLn $ "Send to " <> whichText <> ": " <> msg
      WS.sendTextData conn msg
    Nothing ->
      T.putStrLn $ "No " <> whichText <> "to send to."
  where
    whichText = cs (show which) :: Text

sendToSpecs :: Text -> Room -> IO ()
sendToSpecs msg room = do
  T.putStrLn ("To Specs: " <> msg)
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

  msg <- WS.receiveData conn
  roomName <- return "default"
  roomVar <- getRoom roomName state
  initialRoom <- readMVar roomVar


  case msg of
    _ | not valid ->
        WS.sendTextData conn $ toChat $ ErrorCommand $ "Connection protocol failure" <> msg

      | any ($ fst client) [ T.null ] ->
        WS.sendTextData conn $ toChat $ ErrorCommand "Name must be nonempty"

      | clientExists client initialRoom ->
        WS.sendTextData conn $ toChat $ ErrorCommand "User already exists"

      | prefix == "play:" -> do
        if not (roomFull initialRoom) then
          do
            flip finally (disconnect client roomVar roomName state) (play client roomVar)
            else
              WS.sendTextData conn $ toChat $ ErrorCommand $ "Room is full :("

      | prefix == "spectate:" ->
        flip finally (disconnect client roomVar roomName state) (spectate client roomVar)

      | otherwise ->
        WS.sendTextData conn $ toChat $ ErrorCommand "Something went terribly wrong in connection negotiation :("

      where
        (valid, prefix) = validConnectMsg msg :: (Bool, Text)
        client = (T.drop (T.length prefix) msg, conn) :: Client

spectate :: Client -> MVar Room -> IO ()
spectate client@(user, conn) room = do
    room' <- addSpecClient client room
    WS.sendTextData conn ("acceptSpec:" :: Text)
    WS.sendTextData conn $ "chat:Welcome! " <> userList room'
    broadcast (toChat (SpectateCommand (fst client))) room'
    syncClients room'
    forever $ do
      msg <- WS.receiveData conn
      actSpec (parseMsg user msg) room

play :: Client -> MVar Room -> IO ()
play client@(user, conn) room = do
    (room', which) <- addPlayerClient client room
    WS.sendTextData conn ("acceptPlay:" :: Text)
    WS.sendTextData conn ("chat:Welcome! " <> userList room')
    broadcast (toChat (PlayCommand (fst client))) room'
    syncClients room'
    forever $ do
      msg <- WS.receiveData conn
      actPlay (parseMsg user msg) which room


disconnect :: Client -> MVar Room -> RoomName -> MVar ServerState -> IO (ServerState)
disconnect client room name state = do
  r <- removeClient client room
  broadcast (toChat (LeaveCommand (fst client))) r
  if roomEmpty r then
    do
      s' <- deleteRoom name state
      return s'
      else
        do
          s' <- readMVar state
          return s'

validConnectMsg :: Text -> (Bool, Text)
validConnectMsg msg
  | T.isPrefixOf "spectate:" msg = (True, "spectate:")
  | T.isPrefixOf "play:" msg = (True, "play:")
  | otherwise = (False, "")

userList :: Room -> Text
userList room
  | users == "" = "You're the only one here..."
  | otherwise = "Users: " <> users
  where
    users :: Text
    users = T.intercalate ", " (map fst (getRoomClients room))

actPlay :: Command -> WhichPlayer -> MVar Room -> IO ()
actPlay cmd which room =
  case trans cmd of
    Just command ->
      do
        r <- stateUpdate command which room
        syncClients r
    Nothing ->
      actSpec cmd room
  where
    trans :: Command -> Maybe GameCommand
    trans DrawCommand = Just Draw
    trans EndTurnCommand = Just EndTurn
    trans (PlayCardCommand name) = Just (PlayCard name)
    trans _ = Nothing

actSpec :: Command -> MVar Room -> IO ()
actSpec cmd room = do
  r <- readMVar room
  broadcast (toChat cmd) r

syncClients :: Room -> IO ()
syncClients room = do
  sendToPlayer PlayerA syncMsgPa room
  sendToPlayer PlayerB syncMsgPb room
  sendToSpecs syncMsgPa room
  where
    game = getRoomModel room :: Model
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
parseMsg _ ""           = ErrorCommand "Command not found"
parseMsg name msg
  | (command == "draw") = DrawCommand
  | (command == "end")  = EndTurnCommand
  | (command == "play") = PlayCardCommand content
  | (command == "chat") = ChatCommand name content
  | otherwise           = ErrorCommand "Unknown command"
  where
    parsed :: (Text, Text)
    parsed = T.breakOn ":" msg
    command = fst parsed :: Text
    content = T.drop 1 (snd parsed) :: Text
