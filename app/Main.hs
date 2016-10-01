{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (lookup)

import Data.Char (isPunctuation, isSpace)
import Data.Map.Strict (Map, delete, empty, insert, lookup)
import Data.Monoid ((<>))
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS


type Username = Text
type Client = (Username, WS.Connection)

type ServerState = Map RoomName Room
type RoomName = Text
data Room = Room [Client] Int

data Command =
    ChatCommand Username Text
  | JoinCommand Username
  | LeaveCommand Username
  | IncCommand
  | ErrorCommand Text


newServerState :: ServerState
newServerState = empty


numClients :: RoomName -> ServerState -> Int
numClients name state = length (getRoomClients room)
  where
  room = getRoom name state :: Room


clientExists :: RoomName -> Client -> ServerState -> Bool
clientExists name client state = any ((== fst client) . fst) (getRoomClients room)
  where
  room = getRoom name state :: Room


newRoom :: Room
newRoom = Room [] 0


getRoom :: RoomName -> ServerState -> Room
getRoom name state = makeRoomIfNotExisting existingRoom
  where
  existingRoom = lookup name state :: Maybe Room
  makeRoomIfNotExisting :: Maybe Room -> Room
  makeRoomIfNotExisting (Just room) = room
  makeRoomIfNotExisting Nothing = newRoom


getRoomClients :: Room -> [Client]
getRoomClients (Room clients _) = clients


getRoomCount :: Room -> Int
getRoomCount (Room _ count) = count


incCount :: RoomName -> ServerState -> ServerState
incCount name state = insert name newRoom state
  where
  room = getRoom name state :: Room
  count = getRoomCount room :: Int
  clients = getRoomClients room :: [Client]
  newRoom = Room clients (count + 1) :: Room

-- Add a client (this does not check if the client already exists, you should do
-- this yourself using `clientExists`):

addClient :: RoomName -> Client -> ServerState -> ServerState
addClient name client state = insert name newRoom state
  where
  room = getRoom name state :: Room
  count = getRoomCount room :: Int
  clients = getRoomClients room :: [Client]
  newClients = client:clients :: [Client]
  newRoom = Room newClients count :: Room


removeClient :: RoomName -> Client -> ServerState -> ServerState
removeClient name client state
  | null newClients = delete name state
  | otherwise = insert name newRoom state
  where
  room = getRoom name state :: Room
  count = getRoomCount room :: Int
  clients = getRoomClients room :: [Client]
  newClients :: [Client]
  newClients = filter ((/= fst client) . fst) clients
  newRoom = Room newClients count :: Room


broadcast :: Command -> RoomName -> ServerState -> IO ()
broadcast command name state = do
  T.putStrLn (process command)
  forM_ clients $ \(_, conn) -> WS.sendTextData conn (process command)
  where
  room = getRoom name state :: Room
  clients = getRoomClients room :: [Client]


-- The main function first creates a new state for the server, then spawns the
-- actual server. For this purpose, we use the simple server provided by
-- `WS.runServer`.

main :: IO ()
main = do
   state <- newMVar newServerState
   WS.runServer "0.0.0.0" 9160 $ application state

-- Our main application has the type:

application :: MVar ServerState -> WS.ServerApp

-- Note that `WS.ServerApp` is nothing but a type synonym for
-- `WS.PendingConnection -> IO ()`.

-- Our application starts by accepting the connection. In a more realistic
-- application, you probably want to check the path and headers provided by the
-- pending request.

-- We also fork a pinging thread in the background. This will ensure the connection
-- stays alive on some browsers.

application state pending = do
   conn <- WS.acceptRequest pending
   WS.forkPingThread conn 30

-- When a client is succesfully connected, we read the first Command. This should
-- be in the format of "Hi! I am Jasper", where Jasper is the requested username.

   msg <- WS.receiveData conn
   rooms <- readMVar state
   case msg of

-- Check that the first Command has the right format:

       _   | not (prefix `T.isPrefixOf` msg) ->
               WS.sendTextData conn (process (ErrorCommand ("Connection protocol failure" :: Text)))

-- Check the validity of the username:

           | any ($ fst client)
               [T.null, T.any isPunctuation, T.any isSpace] ->
                   WS.sendTextData conn (process (ErrorCommand ("Name cannot " <>
                       "contain punctuation or whitespace, and " <>
                       "cannot be empty" :: Text)))

-- Check that the given username is not already taken:

           | clientExists "default" client rooms ->
               WS.sendTextData conn (process (ErrorCommand ("User already exists" :: Text)))

-- All is right! We're going to allow the client, but for safety reasons we *first*
-- setup a `disconnect` function that will be run when the connection is closed.

           | otherwise -> flip finally disconnect $ do

-- We send a "Welcome!", according to our own little protocol. We add the client to
-- the list and broadcast the fact that he has joined. Then, we give control to the
-- 'gameloop' function.

              modifyMVar_ state $ \s -> do
                  let s' = addClient "default" client s
                  WS.sendTextData conn $
                      "Welcome! " <> userList s
                  broadcast (JoinCommand (fst client)) "default" s'
                  return s'
              gameLoop conn state client
         where
          prefix     = "join:"
          client     = (T.drop (T.length prefix) msg, conn)
          disconnect = do
              -- Remove client and return new state
              s <- modifyMVar state $ \s ->
                  let s' = removeClient "default" client s in return (s', s')
              broadcast (LeaveCommand (fst client)) "default" s

userList :: ServerState -> Text
userList s
  | (users == "") = "You're the only one here..."
  | otherwise     = "Users: " <> users
  where
  users :: Text
  users = T.intercalate ", " (map fst  (getRoomClients (getRoom "default" s)))

gameLoop :: WS.Connection -> MVar ServerState -> Client -> IO ()
gameLoop conn state (user, _) = forever $ do
  msg <- WS.receiveData conn
  s <- modifyMVar state $ \x -> do
    let s' = incCount "default" x
    return (s', s')
  talk (parsedInput msg) s
  where
  parsedInput :: Text -> Command
  parsedInput msg = parseMsg user msg

talk :: Command -> ServerState -> IO ()
talk cmd state = do
  broadcast cmd "default" state
  where
  countText :: ServerState -> Text
  countText s = "(count: " <> (T.pack $ show $ getRoomCount $ getRoom "default" s) <> ")"

process :: Command -> Text
process (JoinCommand name)         = name <> " joined"
process (LeaveCommand name)        = name <> " disconnected"
process (ChatCommand name message) = name <> ": " <> message
process (IncCommand)               = "Count incremented"
process (ErrorCommand err)  = err

parseMsg :: Username -> Text -> Command
parseMsg _ ""           = ErrorCommand "Command not found"
parseMsg name msg
  | (command == "inc")  = IncCommand
  | (command == "chat") = ChatCommand name content
  | otherwise           = ErrorCommand "Unknown command"
  where
  parsed :: (Text, Text)
  parsed = T.breakOn ":" msg
  command = fst parsed :: Text
  content = T.drop 1 (snd parsed) :: Text
