module Server where

import Client (Client)
import qualified Client
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar)
import Control.Monad.STM (STM)
import Data.Map.Strict (Map, delete, elems, empty, insert, keys, lookup)
import Data.Text (Text)
import GameState (WaitType (..))
import Outcome (Outcome)
import Player (WhichPlayer (..))
import Room (Room (..))
import qualified Room
import Scenario (Scenario (..), markCpu)
import Stats.Progress (Progress)
import Util (Gen, modReadTVar, modReturnTVar, modTVar)
import Prelude hiding (lookup, putStrLn)

data State = State
  { state_rooms :: Map Room.Name (TVar Room),
    state_matching :: MatchingQueue
  }

instance Show State where
  show state = show . keys $ state_rooms state

initState :: State
initState =
  State
    { state_rooms = empty,
      state_matching = []
    }

-- GETTING / DELETING ROOMS
getRoom :: Room.Name -> TVar State -> STM (Maybe (TVar Room))
getRoom name state =
  lookup name . state_rooms <$> readTVar state

createRoom :: Room.Name -> TVar Room -> TVar State -> STM (TVar Room)
createRoom name roomVar state =
  modReturnTVar
    state
    (\(State s m) -> (State (insert name roomVar s) m, roomVar))

getOrCreateRoom :: Room.Name -> WaitType -> Gen -> Scenario -> TVar State -> STM (TVar Room)
getOrCreateRoom name wait gen scenario state = do
  newRoomVar <- newTVar $ Room.new wait gen name scenario
  existingRoomVar <- getRoom name state
  case existingRoomVar of
    Nothing ->
      createRoom name newRoomVar state
    Just r ->
      return r

deleteRoom :: Room.Name -> TVar State -> STM State
deleteRoom name state =
  modReadTVar state $ \(State s qs) -> State (delete name s) qs

getAllRoomVars :: TVar State -> STM [TVar Room]
getAllRoomVars state = elems . state_rooms <$> readTVar state

getAllRooms :: TVar State -> STM [Room]
getAllRooms state = do
  roomVars <- getAllRoomVars state
  mapM readTVar roomVars

-- QUEUEING
type MatchingQueue = [(Text, TVar Room)]

queue :: Text -> TVar Room -> TVar State -> STM (Maybe (TVar Room))
queue newQueueId roomVar state = do
  matching <- state_matching <$> readTVar state
  case matching of
    (existingQueueId, existingRoomVar) : _ -> do
      dequeue existingQueueId state
      return $ Just existingRoomVar
    _ -> do
      _ <- modTVar state (\s -> s {state_matching = (newQueueId, roomVar) : matching})
      return Nothing

dequeue :: Text -> TVar State -> STM ()
dequeue queueId state = do
  modTVar
    state
    ( \s ->
        s
          { state_matching = filter (\(qid, _) -> qid /= queueId) $ state_matching s
          }
    )
  return ()

modScenario :: (Scenario -> Scenario) -> TVar Room -> STM Room
modScenario f roomVar =
  modReadTVar roomVar $ Room.modScenario f

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

addComputerClient :: Text -> Text -> Progress -> TVar Room -> STM (Maybe (Client, [Outcome]))
addComputerClient name guid progress room =
  modReturnTVar room $ \r ->
    if Room.noCpus r
      then case Room.addPlayer client r of
        Just (newRoom, outcomes, _) ->
          (newRoom {room_scenario = markCpu $ room_scenario newRoom}, Just (client, outcomes))
        Nothing ->
          (r, Nothing)
      else (r, Nothing)
  where
    client = Client.cpuClient name guid progress :: Client

removeClient :: Client -> TVar Room -> STM Room
removeClient client roomVar =
  modReadTVar roomVar $ Room.removeClient client
