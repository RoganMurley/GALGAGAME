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
getRoom name state = modifyMVar_ state $ \s ->
  do
    case lookup name s of
      Just room ->
        return s
      Nothing ->
        return (insert name (newMVar newRoom) s)

stateUpdate :: GameCommand -> WhichPlayer -> MVar Room -> IO (MVar Room)
stateUpdate cmd which room =
  return $ modifyMVar room $ \r -> return (gameUpdate cmd r, gameUpdate cmd r)
  where
    gameUpdate :: GameCommand -> Room -> Room
    gameUpdate cmd (Room pa pb specs model) = Room pa pb specs (fromMaybe model (update cmd which model)) -- LOOKS DANGEROUS?

addSpecClient :: Client -> MVar Room -> IO ()
addSpecClient client room = modifyMVar_ room (\r -> return (addSpec client r))

addPlayerClient :: Client -> MVar Room -> IO ()
addPlayerClient client room = modifyMVar_ room (\r -> return (addPlayer client r))

removeClient :: Client -> MVar Room -> IO ()
removeClient client room = modifyMVar_ room (\r -> return (removeClientRoom client r))

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
  roomVar <- getRoom "default" state
  initialRoom <- readMVar roomVar

  case msg of
    _ | not valid ->
        WS.sendTextData conn (toChat (ErrorCommand ("Connection protocol failure" <> msg :: Text)))

      | any ($ fst client) [ T.null ] ->
        WS.sendTextData conn (toChat (ErrorCommand ("Name must be nonempty" :: Text)))

      | clientExists client initialRoom ->
        WS.sendTextData conn (toChat (ErrorCommand ("User already exists" :: Text)))

      | prefix == "play:" -> do
        -- Switch to a modify again in the future.
        r <- takeMVar roomVar
        if not (roomFull initialRoom) then
          do
            flip finally (disconnect client state) $ do
              let (r', which) = addPlayerClient client r
              putMVar roomVar r'
              WS.sendTextData conn ("acceptPlay:" :: Text)
              WS.sendTextData conn ("chat:Welcome! " <> userList r')
              broadcast (toChat (PlayCommand (fst client))) r'
              syncClients r'
              playLoop conn r' client which
            else
              do
                WS.sendTextData conn (toChat (ErrorCommand ("Room is full :(" :: Text)))
                putMVar roomVar r

      | prefix == "spectate:" -> flip finally (disconnect client state) $ do
        modifyMVar_ room $ \r ->
          do
            let r' = addSpecClient client r
            WS.sendTextData conn ("acceptSpec:" :: Text)
            WS.sendTextData conn ("chat:Welcome! " <> userList r')
            broadcast (toChat (SpectateCommand (fst client))) r'
            syncClients r'
            return r'
        specLoop conn room client

      | otherwise ->
        WS.sendTextData conn (toChat (ErrorCommand ("Something went terribly wrong in connection negotiation :(" :: Text)))

      where
        (valid, prefix) = validConnectMsg msg :: (Bool, Text)
        client = (T.drop (T.length prefix) msg, conn) :: Client

disconnect :: Client -> MVar ServerState -> IO ()
disconnect client state = do
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

playLoop :: WS.Connection -> MVar Room -> Client -> WhichPlayer -> IO ()
playLoop conn room (user, _) which = forever $ do
  msg <- WS.receiveData conn
  actPlay (parseMsg user msg) which room

specLoop :: WS.Connection -> MVar Room -> Client -> IO ()
specLoop conn room (user, _) = forever $ do
  msg <- WS.receiveData conn
  actSpec (parseMsg user msg) room

actPlay :: Command -> WhichPlayer -> MVar Room -> IO ()
actPlay cmd which room =
  case trans cmd of
    Just command ->
      modifyMVar_ room $ \r ->
        do
          r' <- stateUpdate command which r
          syncClients r'
          return r'
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
