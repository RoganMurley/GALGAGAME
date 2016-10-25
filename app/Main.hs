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

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import GameState (CardName, GameCommand(..), Model(..), WhichPlayer(..), reverso, update)
import Room


type ServerState = Map RoomName Room

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

getRoom :: RoomName -> ServerState -> Room
getRoom name state = getOrCreate existingRoom
  where
  existingRoom = lookup name state :: Maybe Room
  getOrCreate :: Maybe Room -> Room
  getOrCreate (Just room) = room
  getOrCreate Nothing = newRoom

stateUpdate :: GameCommand -> WhichPlayer -> RoomName -> ServerState -> ServerState
stateUpdate cmd which name state = insert name newRoom state
  where
  room = getRoom name state :: Room
  newRoom = gameUpdate cmd room :: Room
  gameUpdate :: GameCommand -> Room -> Room
  gameUpdate cmd (Room pa pb specs model) = Room pa pb specs (fromMaybe model (update cmd which model))


-- Add a client (this does not check if the client already exists, you should do
-- this yourself using `clientExists`):

addSpecClient :: RoomName -> Client -> ServerState -> ServerState
addSpecClient name client state = insert name newRoom state
  where
  room = getRoom name state :: Room
  newRoom = addSpec client room :: Room

addPlayerClient :: RoomName -> Client -> ServerState -> (ServerState, WhichPlayer)
addPlayerClient name client state = (insert name newRoom state, which)
  where
  room = getRoom name state :: Room
  (newRoom, which) = addPlayer client room :: (Room, WhichPlayer)

removeClient :: RoomName -> Client -> ServerState -> ServerState
removeClient name client state
  | null newClients = delete name state
  | otherwise = insert name newRoom state
  where
  room = getRoom name state :: Room
  newRoom = removeClientRoom client room :: Room
  newClients = getRoomClients newRoom :: [Client]


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

main :: IO ()
main = do
   state <- newMVar newServerState
   WS.runServer "0.0.0.0" 9160 $ application state


application :: MVar ServerState -> WS.ServerApp
application state pending = do
   conn <- WS.acceptRequest pending
   WS.forkPingThread conn 30

   msg <- WS.receiveData conn
   rooms <- readMVar state
   case msg of

       _   | not valid ->
               WS.sendTextData conn (toChat (ErrorCommand ("Connection protocol failure" <> msg :: Text)))

           | any ($ fst client)
               [ T.null ] ->
                   WS.sendTextData conn (toChat (ErrorCommand ("Name must be nonempty" :: Text)))


           | clientExists client (getRoom "default" rooms) ->
               WS.sendTextData conn (toChat (ErrorCommand ("User already exists" :: Text)))

           | prefix == "play:" -> do
              s <- takeMVar state
              if not (roomFull (getRoom "default" s)) then
                do
                  flip finally disconnect $ do
                    let (s', which) = addPlayerClient "default" client s
                    let room = getRoom "default" s
                    putMVar state s'
                    WS.sendTextData conn ("acceptPlay:" :: Text)
                    WS.sendTextData conn ("chat:Welcome! " <> userList room)
                    broadcast (toChat (PlayCommand (fst client))) room
                    syncClients room
                    playLoop conn state client which
                  else
                    do
                      WS.sendTextData conn (toChat (ErrorCommand ("Room is full :(" :: Text)))
                      putMVar state s

           | prefix == "spectate:" -> flip finally disconnect $ do
              modifyMVar_ state $ \s -> do
                  let s' = addSpecClient "default" client s
                  let room = getRoom "default" s
                  WS.sendTextData conn ("acceptSpec:" :: Text)
                  WS.sendTextData conn ("chat:Welcome! " <> userList room)
                  broadcast (toChat (SpectateCommand (fst client))) room
                  syncClients room
                  return s'
              specLoop conn state client

           | otherwise -> WS.sendTextData conn (toChat (ErrorCommand ("Something went terribly wrong in connection negotiation :(" :: Text)))

         where
          (valid, prefix) = validConnectMsg msg
          client = (T.drop (T.length prefix) msg, conn)
          disconnect = do
              s <- modifyMVar state $ \s ->
                  let s' = removeClient "default" client s in return (s', s')
              broadcast (toChat (LeaveCommand (fst client))) (getRoom "default" s)

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

playLoop :: WS.Connection -> MVar ServerState -> Client -> WhichPlayer -> IO ()
playLoop conn state (user, _) which = forever $ do
  msg <- WS.receiveData conn
  actPlay (parseMsg user msg) which state

specLoop :: WS.Connection -> MVar ServerState -> Client -> IO ()
specLoop conn state (user, _) = forever $ do
  msg <- WS.receiveData conn
  actSpec (parseMsg user msg) state

actPlay :: Command -> WhichPlayer -> MVar ServerState -> IO ()
actPlay cmd which state =
  case trans cmd of
    Just command ->
      do
        s <- modifyMVar state $ \x -> do
          let s' = stateUpdate command which "default" x
          return (s', s')
        syncClients (getRoom "default" s)
    Nothing ->
      actSpec cmd state
  where
    trans :: Command -> Maybe GameCommand
    trans DrawCommand = Just Draw
    trans EndTurnCommand = Just EndTurn
    trans (PlayCardCommand name) = Just (PlayCard name)
    trans _ = Nothing

actSpec :: Command -> MVar ServerState -> IO ()
actSpec cmd state = do
    s <- readMVar state
    broadcast (toChat cmd) (getRoom "default" s)

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
