module Server where

import Prelude hiding (lookup, putStrLn)

import Control.Monad.STM (STM)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar)
import Data.Map.Strict (Map, delete, elems, empty, insert, keys, lookup)
import Data.Text (Text)

import GameState (WaitType(..))
import Outcome (Outcome)
import Player (WhichPlayer(..))
import Scenario (Scenario(..))
import Util (Gen, modReadTVar, modReturnTVar, modTVar)

import qualified Client
import Client (Client)

import qualified Room
import Room (Room)


data State = State
  { state_rooms    :: Map Room.Name (TVar Room)
  , state_matching :: Maybe (TVar Room)
  , state_worldPvp :: Maybe (TVar Room)
  }


instance Show State where
  show state = show . keys $ state_rooms state


initState :: State
initState = State empty Nothing Nothing


-- GETTING / DELETING ROOMS
getRoom :: Room.Name -> TVar State -> STM (Maybe (TVar Room))
getRoom name state =
  (lookup name) . state_rooms <$> readTVar state


createRoom :: Room.Name -> TVar Room -> TVar State -> STM (TVar Room)
createRoom name roomVar state =
  modReturnTVar
    state
    (\(State s m w) -> (State (insert name roomVar s) m w, roomVar))


getOrCreateRoom :: Room.Name -> WaitType -> Gen -> Scenario -> TVar State -> STM (TVar Room)
getOrCreateRoom name wait gen scenario state = do
  newRoomVar      <- newTVar $ Room.new wait gen name scenario
  existingRoomVar <- getRoom name state
  case existingRoomVar of
    Nothing ->
      createRoom name newRoomVar state
    Just r ->
      return r


deleteRoom :: Room.Name -> TVar State -> STM (State)
deleteRoom name state =
  modReadTVar state $ \(State s qs r) -> State (delete name s) qs r


getAllRooms :: TVar State -> STM ([TVar Room])
getAllRooms state = elems . state_rooms <$> readTVar state


-- QUEUEING
queue :: TVar Room -> TVar State -> STM (Maybe (TVar Room))
queue roomVar state = do
    match <- state_matching <$> readTVar state
    case match of
      Just existingRoomVar -> do
        _ <- modTVar state (\(State s _ w) -> State s Nothing w)
        return $ Just existingRoomVar
      Nothing -> do
        _ <- modTVar state (\(State s _ w) -> (State s (Just roomVar) w))
        return Nothing


modScenario :: (Scenario -> Scenario) -> TVar Room -> STM Room
modScenario f roomVar =
  modReadTVar roomVar $ Room.modScenario f


-- WORLD PVP
getWorldPvpRoom :: TVar State -> STM (Maybe (TVar Room))
getWorldPvpRoom stateVar = do
  state <- readTVar stateVar
  return $ state_worldPvp state


setWorldPvpRoom :: TVar State -> Maybe (TVar Room) -> STM ()
setWorldPvpRoom stateVar mRoomVar =
  modTVar stateVar (\state -> state { state_worldPvp = mRoomVar })


-- ADDING/REMOVING CLIENTS.
addSpecClient :: Client -> TVar Room -> STM Room
addSpecClient client roomVar =
  modReadTVar roomVar $ Room.addSpec client


addPlayerClient :: Client -> TVar Room -> STM (Maybe (WhichPlayer, [Outcome]))
addPlayerClient client roomVar =
  modReturnTVar roomVar $ \room ->
    case Room.addPlayer client room of
      Just (room', outcomes, which) ->
        (room', Just (which, outcomes))
      Nothing ->
        (room, Nothing)


addComputerClient :: Text -> Text -> TVar Room -> STM (Maybe Client)
addComputerClient name guid room =
  modReturnTVar room $ \r ->
    case Room.addPlayer client r of
      Just (r', _, _) ->
        (r', Just client)
      Nothing ->
        (r, Nothing)
  where
    client = Client.cpuClient name guid :: Client


removeClient :: Client -> TVar Room -> STM (Room)
removeClient client roomVar =
  modReadTVar roomVar $ Room.removeClient client
