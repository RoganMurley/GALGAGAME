{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (lookup)

import Data.Char (isPunctuation, isSpace)
import Data.Map.Strict (Map, delete, empty, insert, lookup)
import Data.Maybe (maybeToList)
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
type Player = Maybe Client
type Spectators = [Client]
type GameState = [Int]
data Room = Room Player Player Spectators GameState

data Command =
    ChatCommand Username Text
  | SpectateCommand Username
  | LeaveCommand Username
  | IncCommand
  | ErrorCommand Text


newServerState :: ServerState
newServerState = empty


clientExists :: RoomName -> Client -> ServerState -> Bool
clientExists name client state = any ((== fst client) . fst) (getRoomClients room)
  where
  room = getRoom name state :: Room


newRoom :: Room
newRoom = Room Nothing Nothing [] [ 0 ]


getRoom :: RoomName -> ServerState -> Room
getRoom name state = makeRoomIfNotExisting existingRoom
  where
  existingRoom = lookup name state :: Maybe Room
  makeRoomIfNotExisting :: Maybe Room -> Room
  makeRoomIfNotExisting (Just room) = room
  makeRoomIfNotExisting Nothing = newRoom


getRoomClients :: Room -> [Client]
getRoomClients (Room pa pb specs _) = (maybeToList pa) ++ (maybeToList pb) ++ specs


getRoomCount :: Room -> GameState
getRoomCount (Room _ _ _ game) = game


incCount :: RoomName -> ServerState -> ServerState
incCount name state = insert name newRoom state
  where
  room = getRoom name state :: Room
  newRoom = addCount (length (getRoomCount room)) room :: Room

addCount :: Int -> Room -> Room
addCount n (Room pa pb specs game) = Room pa pb specs (n:game)

-- Add a client (this does not check if the client already exists, you should do
-- this yourself using `clientExists`):

addClient :: RoomName -> Client -> ServerState -> ServerState
addClient name client state = insert name newRoom state
  where
  room = getRoom name state :: Room
  newRoom = addSpec client room :: Room


addSpec :: Client -> Room -> Room
addSpec client (Room pa pb specs count) = Room pa pb (client:specs) count


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
                  WS.sendTextData conn ("accept:" :: Text)
                  WS.sendTextData conn $
                      "chat:Welcome! " <> userList s
                  broadcast (process (SpectateCommand (fst client))) "default" s'
                  syncClients s'
                  return s'
              gameLoop conn state client
         where
          prefix     = "spectate:"
          client     = (T.drop (T.length prefix) msg, conn)
          disconnect = do
              -- Remove client and return new state
              s <- modifyMVar state $ \s ->
                  let s' = removeClient "default" client s in return (s', s')
              broadcast (process (LeaveCommand (fst client))) "default" s

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
  act (parsedInput msg) state
  where
  parsedInput :: Text -> Command
  parsedInput msg = parseMsg user msg

act :: Command -> MVar ServerState -> IO ()
act cmd@IncCommand state = do
    s <- modifyMVar state $ \x -> do
      let s' = incCount "default" x
      return (s', s')
    syncClients s
act cmd state = do
    s <- readMVar state
    broadcast (process cmd) "default" s

syncClients :: ServerState -> IO ()
syncClients state = broadcast syncMsg "default" state
  where
  room = getRoom "default" state :: Room
  game = getRoomCount room :: GameState
  syncMsg = "sync:" <> (countText state) :: Text
  quoteWrap :: Text -> Text
  quoteWrap t = "\"" <> t <> "\""
  countText :: ServerState -> Text
  countText state = "[" <> (T.intercalate "," (fmap (quoteWrap . T.pack . show) game)) <> "]"

process :: Command -> Text
process (SpectateCommand name)     = "chat:" <> name <> " started spectating"
process (LeaveCommand name)        = "chat:" <> name <> " disconnected"
process (ChatCommand name message) = "chat:" <> name <> ": " <> message
process (IncCommand)               = "chat:" <> "Count incremented"
process (ErrorCommand err)         = "chat:" <> err

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
