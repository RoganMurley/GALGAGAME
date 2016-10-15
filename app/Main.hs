{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (lookup)

import Data.Aeson (encode)
import Data.Char (isPunctuation, isSpace)
import Data.Map.Strict (Map, delete, empty, insert, lookup)
import Data.Maybe (maybeToList)
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

import GameState (GameCommand(..), Model(..), initModel, update)


type Username = Text
type Client = (Username, WS.Connection)

type ServerState = Map RoomName Room
type RoomName = Text
type Player = Maybe Client
type Spectators = [Client]

data Room = Room Player Player Spectators Model

data Command =
    ChatCommand Username Text
  | PlayCommand Username
  | SpectateCommand Username
  | LeaveCommand Username
  | DrawCommand
  | ErrorCommand Text


newServerState :: ServerState
newServerState = empty


clientExists :: RoomName -> Client -> ServerState -> Bool
clientExists name client state = any ((== fst client) . fst) (getRoomClients room)
  where
  room = getRoom name state :: Room


newRoom :: Room
newRoom = Room Nothing Nothing [] initModel


getRoom :: RoomName -> ServerState -> Room
getRoom name state = makeRoomIfNotExisting existingRoom
  where
  existingRoom = lookup name state :: Maybe Room
  makeRoomIfNotExisting :: Maybe Room -> Room
  makeRoomIfNotExisting (Just room) = room
  makeRoomIfNotExisting Nothing = newRoom


getRoomClients :: Room -> [Client]
getRoomClients (Room pa pb specs _) = (maybeToList pa) ++ (maybeToList pb) ++ specs


getRoomModel :: Room -> Model
getRoomModel (Room _ _ _ model) = model


stateUpdate :: GameCommand -> RoomName -> ServerState -> ServerState
stateUpdate cmd name state = insert name newRoom state
  where
  room = getRoom name state :: Room
  newRoom = gameUpdate cmd room :: Room
  gameUpdate :: GameCommand -> Room -> Room
  gameUpdate cmd (Room pa pb specs model) = Room pa pb specs (update cmd model)


-- Add a client (this does not check if the client already exists, you should do
-- this yourself using `clientExists`):

addSpecClient :: RoomName -> Client -> ServerState -> ServerState
addSpecClient name client state = insert name newRoom state
  where
  room = getRoom name state :: Room
  newRoom = addSpec client room :: Room

addPlayerClient :: RoomName -> Client -> ServerState -> ServerState
addPlayerClient name client state = insert name newRoom state
  where
  room = getRoom name state :: Room
  newRoom = addPlayer client room :: Room


addSpec :: Client -> Room -> Room
addSpec client (Room pa pb specs count) = Room pa pb (client:specs) count

addPlayer :: Client -> Room -> Room
addPlayer client (Room Nothing pb specs count) = Room (Just client) pb specs count
addPlayer client (Room pa Nothing specs count) = Room pa (Just client) specs count
addPlayer client (Room (Just a) (Just b) specs count) = Room (Just a) (Just b) specs count


roomFull :: Room -> Bool
roomFull (Room (Just _) (Just _) _ _) = True
roomFull _ = False


removeClient :: RoomName -> Client -> ServerState -> ServerState
removeClient name client state
  | null newClients = delete name state
  | otherwise = insert name newRoom state
  where
  room = getRoom name state :: Room
  newRoom = removeClientRoom client room :: Room
  newClients = getRoomClients newRoom :: [Client]

removeClientRoom :: Client -> Room -> Room
removeClientRoom client (Room pa pb specs count) =
  Room (newPlayer pa) (newPlayer pb) newSpecs count
  where
  newSpecs :: Spectators
  newSpecs = filter ((/= fst client) . fst) specs
  newPlayer :: Player -> Player
  newPlayer Nothing = Nothing
  newPlayer (Just c) = if fst c == fst client then Nothing else Just c


broadcast :: Text -> RoomName -> ServerState -> IO ()
broadcast msg name state = do
  T.putStrLn msg
  forM_ clients $ \(_, conn) -> WS.sendTextData conn msg
  where
  room = getRoom name state :: Room
  clients = getRoomClients room :: [Client]


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
               WS.sendTextData conn (process (ErrorCommand ("Connection protocol failure" <> msg :: Text)))

           | any ($ fst client)
               [ T.null ] ->
                   WS.sendTextData conn (process (ErrorCommand ("Name must be nonempty" :: Text)))


           | clientExists "default" client rooms ->
               WS.sendTextData conn (process (ErrorCommand ("User already exists" :: Text)))

           | prefix == "play:" -> do
              s <- takeMVar state
              if not (roomFull (getRoom "default" s)) then
                do
                  flip finally disconnect $ do
                    let s' = addPlayerClient "default" client s
                    putMVar state s'
                    WS.sendTextData conn ("acceptPlay:" :: Text)
                    WS.sendTextData conn ("chat:Welcome! " <> userList s)
                    broadcast (process (PlayCommand (fst client))) "default" s'
                    syncClients s'
                    playLoop conn state client
                  else
                    do
                      WS.sendTextData conn (process (ErrorCommand ("Room is full :(" :: Text)))
                      putMVar state s

           | prefix == "spectate:" -> flip finally disconnect $ do
              modifyMVar_ state $ \s -> do
                  let s' = addSpecClient "default" client s
                  WS.sendTextData conn ("acceptSpec:" :: Text)
                  WS.sendTextData conn $
                       "chat:Welcome! " <> userList s
                  broadcast (process (SpectateCommand (fst client))) "default" s'
                  syncClients s'
                  return s'
              specLoop conn state client

           | otherwise -> WS.sendTextData conn (process (ErrorCommand ("Something went terribly wrong in connection negotiation :(" :: Text)))

         where
          (valid, prefix) = validConnectMsg msg
          client = (T.drop (T.length prefix) msg, conn)
          disconnect = do
              -- Remove client and return new state
              s <- modifyMVar state $ \s ->
                  let s' = removeClient "default" client s in return (s', s')
              broadcast (process (LeaveCommand (fst client))) "default" s

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

playLoop :: WS.Connection -> MVar ServerState -> Client -> IO ()
playLoop conn state (user, _) = forever $ do
  msg <- WS.receiveData conn
  actPlay (parsedInput msg) state
  where
  parsedInput :: Text -> Command
  parsedInput msg = parseMsg user msg

specLoop :: WS.Connection -> MVar ServerState -> Client -> IO ()
specLoop conn state (user, _) = forever $ do
  msg <- WS.receiveData conn
  actSpec (parsedInput msg) state
  where
  parsedInput :: Text -> Command
  parsedInput msg = parseMsg user msg

actPlay :: Command -> MVar ServerState -> IO ()
actPlay cmd@DrawCommand state = do
    s <- modifyMVar state $ \x -> do
      let s' = stateUpdate Draw "default" x
      return (s', s')
    syncClients s
actPlay cmd state = actSpec cmd state

actSpec :: Command -> MVar ServerState -> IO ()
actSpec cmd state = do
    s <- readMVar state
    broadcast (process cmd) "default" s

syncClients :: ServerState -> IO ()
syncClients state = broadcast syncMsg "default" state
  where
  room = getRoom "default" state :: Room
  game = getRoomModel room :: Model
  syncMsg = "sync:" <> (cs $ encode $ game) :: Text


process :: Command -> Text
process (SpectateCommand name)     = "chat:" <> name <> " started spectating"
process (PlayCommand name)         = "chat:" <> name <> " started playing"
process (LeaveCommand name)        = "chat:" <> name <> " disconnected"
process (ChatCommand name message) = "chat:" <> name <> ": " <> message
process (ErrorCommand err)         = "error:" <> err
process _                          = "chat:" <> "Command cannot be processed to text :/"


parseMsg :: Username -> Text -> Command
parseMsg _ ""           = ErrorCommand "Command not found"
parseMsg name msg
  | (command == "draw") = DrawCommand
  | (command == "chat") = ChatCommand name content
  | otherwise           = ErrorCommand "Unknown command"
  where
  parsed :: (Text, Text)
  parsed = T.breakOn ":" msg
  command = fst parsed :: Text
  content = T.drop 1 (snd parsed) :: Text
