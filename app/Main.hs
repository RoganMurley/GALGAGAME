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

import GameState (GameCommand(..), Model(..), WhichPlayer(..), reverso, update)
import Room


type ServerState = Map RoomName Room

data Command =
    ChatCommand Username Text
  | PlayCommand Username
  | SpectateCommand Username
  | LeaveCommand Username
  | DrawCommand
  | EndTurnCommand
  | ErrorCommand Text


newServerState :: ServerState
newServerState = empty

clientExists :: RoomName -> Client -> ServerState -> Bool
clientExists name client state = any ((== fst client) . fst) (getRoomClients room)
  where
  room = getRoom name state :: Room

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


broadcast :: Text -> RoomName -> ServerState -> IO ()
broadcast msg name state = do
  T.putStrLn msg
  forM_ clients $ \(_, conn) -> WS.sendTextData conn msg
  where
  room = getRoom name state :: Room
  clients = getRoomClients room :: [Client]

sendToPlayer :: WhichPlayer -> Text -> RoomName -> ServerState -> IO ()
sendToPlayer PlayerA msg name state = do
  T.putStrLn ("To PlayerA: " <> msg)
  forM_ clients $ \(_, conn) -> WS.sendTextData conn msg
  where
  room = getRoom name state :: Room
  clients = maybeToList (getPlayerClient PlayerA room) :: [Client]
sendToPlayer PlayerB msg name state = do
  T.putStrLn ("To PlayerB: " <> msg)
  forM_ clients $ \(_, conn) -> WS.sendTextData conn msg
  where
  room = getRoom name state :: Room
  clients = maybeToList (getPlayerClient PlayerB room) :: [Client]

sendToSpecs :: Text -> RoomName -> ServerState -> IO ()
sendToSpecs msg name state = do
  T.putStrLn ("To Specs: " <> msg)
  forM_ clients $ \(_, conn) -> WS.sendTextData conn msg
  where
  room = getRoom name state :: Room
  clients = getRoomSpecs room :: [Client]


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


           | clientExists "default" client rooms ->
               WS.sendTextData conn (toChat (ErrorCommand ("User already exists" :: Text)))

           | prefix == "play:" -> do
              s <- takeMVar state
              if not (roomFull (getRoom "default" s)) then
                do
                  flip finally disconnect $ do
                    let (s', which) = addPlayerClient "default" client s
                    putMVar state s'
                    WS.sendTextData conn ("acceptPlay:" :: Text)
                    WS.sendTextData conn ("chat:Welcome! " <> userList s)
                    broadcast (toChat (PlayCommand (fst client))) "default" s'
                    syncClients s'
                    playLoop conn state client which
                  else
                    do
                      WS.sendTextData conn (toChat (ErrorCommand ("Room is full :(" :: Text)))
                      putMVar state s

           | prefix == "spectate:" -> flip finally disconnect $ do
              modifyMVar_ state $ \s -> do
                  let s' = addSpecClient "default" client s
                  WS.sendTextData conn ("acceptSpec:" :: Text)
                  WS.sendTextData conn $
                       "chat:Welcome! " <> userList s
                  broadcast (toChat (SpectateCommand (fst client))) "default" s'
                  syncClients s'
                  return s'
              specLoop conn state client

           | otherwise -> WS.sendTextData conn (toChat (ErrorCommand ("Something went terribly wrong in connection negotiation :(" :: Text)))

         where
          (valid, prefix) = validConnectMsg msg
          client = (T.drop (T.length prefix) msg, conn)
          disconnect = do
              -- Remove client and return new state
              s <- modifyMVar state $ \s ->
                  let s' = removeClient "default" client s in return (s', s')
              broadcast (toChat (LeaveCommand (fst client))) "default" s

validConnectMsg :: Text -> (Bool, Text)
validConnectMsg msg
  | T.isPrefixOf "spectate:" msg = (True, "spectate:")
  | T.isPrefixOf "play:" msg = (True, "play:")
  | otherwise = (False, "")


userList :: ServerState -> Text
userList s
  | (users == "") = "You're the only one here..."
  | otherwise     = "Users: " <> users
  where
  users :: Text
  users = T.intercalate ", " (map fst (getRoomClients (getRoom "default" s)))

playLoop :: WS.Connection -> MVar ServerState -> Client -> WhichPlayer -> IO ()
playLoop conn state (user, _) which = forever $ do
  msg <- WS.receiveData conn
  actPlay (parseMsg user msg) which state

specLoop :: WS.Connection -> MVar ServerState -> Client -> IO ()
specLoop conn state (user, _) = forever $ do
  msg <- WS.receiveData conn
  actSpec (parseMsg user msg) state

actPlay :: Command -> WhichPlayer -> MVar ServerState -> IO ()
actPlay cmd which state
  | isJust (trans cmd) =
    do
      s <- modifyMVar state $ \x -> do
        let s' = stateUpdate (fromJust (trans cmd)) which "default" x
        return (s', s')
      syncClients s
  | otherwise = actSpec cmd state
  where
    trans :: Command -> Maybe GameCommand
    trans DrawCommand = Just (Draw)
    trans EndTurnCommand = Just(EndTurn)
    trans _ = Nothing

actSpec :: Command -> MVar ServerState -> IO ()
actSpec cmd state = do
    s <- readMVar state
    broadcast (toChat cmd) "default" s

syncClients :: ServerState -> IO ()
syncClients state = do
  sendToPlayer PlayerA syncMsgPa "default" state
  sendToPlayer PlayerB syncMsgPb "default" state
  sendToSpecs syncMsgPa "default" state
  where
  room = getRoom "default" state :: Room
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
  | (command == "chat") = ChatCommand name content
  | otherwise           = ErrorCommand "Unknown command"
  where
  parsed :: (Text, Text)
  parsed = T.breakOn ":" msg
  command = fst parsed :: Text
  content = T.drop 1 (snd parsed) :: Text
