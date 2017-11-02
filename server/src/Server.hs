module Server where

import Prelude hiding (lookup, putStrLn)

import Control.Monad.STM (STM)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar)
import Data.Map.Strict (Map, delete, empty, insert, keys, lookup)
import Data.Text (Text)
import Safe (headMay)

import Player (WhichPlayer(..))
import Util (Gen, modReadTVar, modReturnTVar, modTVar)

import qualified Client
import Client (Client)

import qualified Room
import Room (Room)


data State = State
  { state_rooms    :: Map Room.Name (TVar Room)
  , state_matching :: [(Client, TVar Room)]
  }


instance Show State where
  show (State s _) = show . keys $ s


initState :: State
initState = State empty []


-- GETTING / DELETING ROOMS
getRoom :: Room.Name -> TVar State -> STM (Maybe (TVar Room))
getRoom name state =
  (lookup name) . (\(State s _) -> s) <$> readTVar state


createRoom :: Room.Name -> TVar Room -> TVar State -> STM (TVar Room)
createRoom name roomVar state =
  modReturnTVar
    state
    (\(State s m) -> (State (insert name roomVar s) m, roomVar))


getOrCreateRoom :: Room.Name -> Gen -> TVar State -> STM (TVar Room)
getOrCreateRoom name gen state = do
  newRoomVar      <- newTVar $ Room.new gen name
  existingRoomVar <- getRoom name state
  case existingRoomVar of
    Nothing ->
      createRoom name newRoomVar state
    Just r ->
      return r


deleteRoom :: Room.Name -> TVar State -> STM (State)
deleteRoom name state =
  modReadTVar state $ \(State s qs) -> State (delete name s) qs


-- QUEUEING
queue :: (Client, TVar Room) -> TVar State -> STM (Maybe (Client, TVar Room))
queue q state = do
    match <- (\(State _ qs) -> headMay qs) <$> readTVar state
    case match of
      Just (opponent, _) -> do
        _ <- dequeue opponent state
        return match
      Nothing -> do
        modTVar state (\(State s qs) -> (State s (q : qs)))
        return match


dequeue :: Client -> TVar State -> STM (State)
dequeue client state =
  modReadTVar state $
    \(State s qs) -> State s (filter (\(c, _) -> c /= client) qs)



-- ADDING/REMOVING CLIENTS.
addSpecClient :: Client -> TVar Room -> STM Room
addSpecClient client roomVar =
  modReadTVar roomVar $ Room.addSpec client


addPlayerClient :: Client -> TVar Room -> STM (Maybe WhichPlayer)
addPlayerClient client roomVar =
  modReturnTVar roomVar $ \room ->
    case Room.addPlayer client room of
      Just (room', which) ->
        (room', Just (which))
      Nothing ->
        (room, Nothing)


addComputerClient :: Text -> TVar Room -> STM (Maybe Client)
addComputerClient guid room =
  modReturnTVar room $ \r ->
    case Room.addPlayer client r of
      Just (r', _) ->
        (r', Just client)
      Nothing ->
        (r, Nothing)
  where
    client = Client.cpuClient guid :: Client


removeClient :: Client -> TVar Room -> STM (Room)
removeClient client roomVar =
  modReadTVar roomVar $ Room.removeClient client
