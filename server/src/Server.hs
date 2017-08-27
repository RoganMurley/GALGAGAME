module Server where

import Prelude hiding (lookup, putStrLn)

import Control.Monad.STM (STM)
import Control.Concurrent.STM.TVar (TVar, readTVar)
import Data.Map.Strict (Map, delete, empty, insert, keys, lookup)
import Data.Text (Text)

import Player (WhichPlayer(..))
import Util (modReadTVar, modReturnTVar)

import qualified Client
import Client (Client)

import qualified Room
import Room (Room)


newtype State = State (Map Room.Name (TVar Room))


instance Show State where
  show (State rooms) = show . keys $ rooms


initState :: State
initState = State empty


-- GETTING / DELETING ROOMS
getRoom :: Room.Name -> TVar State -> STM (Maybe (TVar Room))
getRoom name state =
  (lookup name) . (\(State s) -> s) <$> readTVar state


createRoom :: Room.Name -> TVar Room -> TVar State -> STM (TVar Room)
createRoom name roomVar state =
  modReturnTVar state (\(State s) -> (State $ insert name roomVar s, roomVar))


getOrCreateRoom :: Room.Name -> TVar Room ->  TVar State -> STM (TVar Room)
getOrCreateRoom name roomVar state = do
  gotRoom <- getRoom name state
  case gotRoom of
    Nothing ->
      createRoom name roomVar state
    Just r ->
      return r


deleteRoom :: Room.Name -> TVar State -> STM (State)
deleteRoom name state =
  modReadTVar state $ \(State s) -> State (delete name s)


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
