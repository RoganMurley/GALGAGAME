
module Main where

import Prelude hiding (lookup)

import Control.Concurrent (MVar, forkIO, newMVar, modifyMVar, readMVar, threadDelay)
import Control.Exception (finally)
import Control.Monad (forM_, forever, mzero, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (encode)
import Data.Map.Strict (Map, delete, empty, insert, lookup)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Safe (readMay)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import ArtificalIntelligence (Action(..), chooseAction)
import Characters (CharModel(..), character_name, toList)
import Model (Model, WhichPlayer(..), getTurn, modelReverso, otherPlayer)
import GameState (GameCommand(..), GameState(..), PlayState(..), Outcome(..), Username, reverso, update)
import qualified Room
import Room (Client, ClientConnection(..), Room, RoomName, sendToClient)
import Util (Err, Gen, getGen, shuffle)


type ServerState = Map RoomName (MVar Room)

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
            r <- newMVar (Room.new gen)
            return (insert name r s, r)

deleteRoom :: RoomName -> MVar ServerState -> IO (ServerState)
deleteRoom name state =
  modifyMVar state $ \s ->
    let s' = delete name s in return (s', s')

stateUpdate :: GameCommand -> WhichPlayer -> MVar Room -> IO (Either Err (Room, [Outcome]))
stateUpdate cmd which room =
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

addSpecClient :: Client -> MVar Room -> IO (Room)
addSpecClient client room =
  modifyMVar room $ \r ->
    let r' = Room.addSpec client r in return (r', r')

addPlayerClient :: Client -> MVar Room -> IO (Maybe (Room, WhichPlayer))
addPlayerClient client room =
  modifyMVar room $ \r ->
    case Room.addPlayer client r of
      Just (r', p) ->
        return (r', Just (r', p))
      Nothing ->
        return (r, Nothing)

addComputerClient :: MVar Room -> IO (Maybe Client)
addComputerClient room =
  modifyMVar room $ \r ->
    case Room.addPlayer client r of
      Just (r', _) ->
        return (r', Just client)
      Nothing ->
        return (r, Nothing)
  where
    client = ("CPU", ComputerConnection) :: Client

removeClient :: Client -> MVar Room -> IO (Room)
removeClient client room =
  modifyMVar room $ \r ->
    let r' = Room.removeClient client r in return (r', r')

broadcast :: Text -> Room -> IO ()
broadcast msg room =
  forM_ (Room.getClients room) (sendToClient msg)

sendToPlayer :: WhichPlayer -> Text -> Room -> IO ()
sendToPlayer which msg room =
  case Room.getPlayerClient which room of
    Just client ->
      sendToClient msg client
    Nothing ->
      return ()

sendToSpecs :: Text -> Room -> IO ()
sendToSpecs msg room =
  forM_ (Room.getSpecs room) (sendToClient msg)

sendExcluding :: WhichPlayer -> Text -> Room -> IO ()
sendExcluding which msg room = do
  sendToSpecs msg room
  sendToPlayer (otherPlayer which) msg room

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
          (WS.sendTextData conn) . toChat $
            ErrorCommand $ "connection protocol failure" <> msg

        Just prefix ->
          case prefix of
            _ | any ($ fst client) [ T.null ] ->
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
                      WS.sendTextData conn $ toChat $ ErrorCommand "room is full"
                    Just (room', which) ->
                      flip finally
                        (disconnect client roomVar roomName state)
                        (play which room' (fst client, conn) roomVar)

              | prefix == "spectate:" ->
                flip finally
                  (disconnect client roomVar roomName state)
                  (spectate (fst client, conn) roomVar)

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
                          _ <- forkIO (computerPlay (otherPlayer which) room' roomVar)
                          play which room' (fst client, conn) roomVar
                        )

              | otherwise ->
                WS.sendTextData conn $ toChat $
                  ErrorCommand "something went wrong with connection negotiation"

              where
                client = (T.drop (T.length prefix) msg, PlayerConnection conn) :: Client
    Nothing ->
      (WS.sendTextData conn) . toChat $
        ErrorCommand "bad room name protocol"

spectate :: (Username, WS.Connection) -> MVar Room -> IO ()
spectate (user, conn) room =
  let
    client = (user, PlayerConnection conn) :: Client
  in
    do
      room' <- addSpecClient client room
      sendToClient ("acceptSpec:" :: Text) client
      sendToClient ("chat:Welcome! " <> userList room') client
      sendToClient ("chat:You're spectating " <> (Room.getSpeccingName room')) client
      broadcast (toChat (SpectateCommand (fst client))) room'
      syncClient client (Room.getState room')
      forever $ do
        msg <- WS.receiveData conn
        actSpec (parseMsg user msg) room

play :: WhichPlayer -> Room -> (Username, WS.Connection) -> MVar Room -> IO ()
play which room' (user, conn) room =
  let
    client = (user, PlayerConnection conn) :: Client
  in
    do
      sendToClient ("acceptPlay:" :: Text) client
      sendToClient ("chat:Welcome! " <> userList room') client
      broadcast (toChat (PlayCommand (fst client))) room'
      syncRoomClients room'
      forever $ do
        msg <- WS.receiveData conn
        actPlay (parseMsg user msg) which room


-- Switch from transformers to mtl?
computerPlay :: WhichPlayer -> Room -> MVar Room -> IO ()
computerPlay which _ room =
  do
  _ <- runMaybeT $ forever $ do
    lift $ putStrLn "AI tick"
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
    (Selecting charModel _ gen) ->
      return . Just . SelectCharacterCommand $ randomChar charModel gen
    (Started (Playing m)) ->
      if getTurn m == which
        then return (Just . trans . chooseAction $ m)
          else return Nothing
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
      character_name . head . (drop (length . Characters.toList $ selected)) $ shuffle allChars gen


disconnect :: Client -> MVar Room -> RoomName -> MVar ServerState -> IO (ServerState)
disconnect client room name state = do
  r <- removeClient client room
  broadcast (toChat (LeaveCommand (fst client))) r
  if Room.empty r then
    deleteRoom name state
      else
        readMVar state

parsePrefix :: Text -> Maybe Text
parsePrefix msg
  | T.isPrefixOf "spectate:" msg = Just "spectate:"
  | T.isPrefixOf "play:" msg = Just "play:"
  | T.isPrefixOf "playComputer:" msg = Just "playComputer:"
  | otherwise = Nothing

userList :: Room -> Text
userList room
  | users == "" = "You're the only one here..."
  | otherwise = "Users: " <> users
  where
    users :: Text
    users = (T.intercalate ", ") . (map fst) $ Room.getClients room

actPlay :: Command -> WhichPlayer -> MVar Room -> IO ()
actPlay cmd which roomVar =
  case trans cmd of
    Just command -> do
      updated <- stateUpdate command which roomVar
      case updated of
        Left err -> do
          room <- readMVar roomVar
          sendToPlayer which (toChat (ErrorCommand err)) room
        Right (room, outcomes) ->
          forM_ outcomes (actOutcome room)
    Nothing ->
      actSpec cmd roomVar
  where
    trans :: Command -> Maybe GameCommand
    trans EndTurnCommand = Just EndTurn
    trans (PlayCardCommand index) = Just (PlayCard index)
    trans (HoverCardCommand index) = Just (HoverCard index)
    trans RematchCommand = Just Rematch
    trans (ChatCommand name content) = Just (Chat name content)
    trans (SelectCharacterCommand n) = Just (SelectCharacter n)
    trans _ = Nothing

actSpec :: Command -> MVar Room -> IO ()
actSpec cmd room = readMVar room >>= broadcast (toChat cmd)

actOutcome :: Room -> Outcome -> IO ()
actOutcome room outcome@(HoverOutcome which _) = do
  sendExcluding which (("hover:" <>) . cs $ encode outcome) room
  T.putStrLn "hovering"
actOutcome room (ChatOutcome username msg) = do
  broadcast ("chat:" <> username <> ": " <> msg) room
  T.putStrLn "chatting"
actOutcome room (ResolveOutcome models final) = do
  resolveRoomClients (models, final) room
  T.putStrLn "resolving"
actOutcome room SyncOutcome = do
  syncRoomClients room
  T.putStrLn "syncing"
actOutcome room (PlayCardOutcome which) = do
  sendExcluding which "playCard:" room
  T.putStrLn "playing card"
actOutcome room (EndTurnOutcome which) = do
  sendExcluding which "end:" room
  T.putStrLn "ending turn"

syncClient :: Client -> GameState -> IO ()
syncClient client game =
  sendToClient (("sync:" <>) . cs . encode $ game :: Text) client

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
    outcome = ResolveOutcome models final :: Outcome
    reversoOutcome = ResolveOutcome (modelReverso <$> models) (reverso final) :: Outcome


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
