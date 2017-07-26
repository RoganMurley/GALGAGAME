module Server where

import Prelude hiding (lookup, putStrLn)

import Control.Concurrent (MVar, newMVar, modifyMVar)
import Data.Map.Strict (Map, delete, empty, insert, keys, lookup)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import Data.String.Conversions (cs)

import Player (WhichPlayer(..))
import Util (getGen)

import qualified Client
import Client (Client)

import qualified Room
import Room (Room)


newtype State = State (Map Room.Name (MVar Room))


instance Show State where
  show (State rooms) = show . keys $ rooms


initState :: State
initState = State empty


-- GETTING / DELETING ROOMS
getRoom :: Room.Name -> MVar State -> IO (MVar Room)
getRoom name state =
  modifyMVar state $ \(State s) ->
      case lookup name s of
        Just room ->
          do
            putStrLn . ("Rooms: " <>) . cs . show . State $ s
            return (State s, room)
        Nothing ->
          do
            gen <- getGen
            r <- newMVar (Room.new gen)
            putStrLn . ("Rooms: " <>) . cs . show . State $ insert name r s
            return (State (insert name r s), r)


deleteRoom :: Room.Name -> MVar State -> IO (State)
deleteRoom name state =
  modifyMVar state $ \(State s) ->
    let s' = State (delete name s) in return (s', s')


-- ADDING/REMOVING CLIENTS.
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


addComputerClient :: Text -> MVar Room -> IO (Maybe Client)
addComputerClient guid room =
  modifyMVar room $ \r ->
    case Room.addPlayer client r of
      Just (r', _) ->
        return (r', Just client)
      Nothing ->
        return (r, Nothing)
  where
    client = Client.cpuClient guid :: Client


removeClient :: Client -> MVar Room -> IO (Room)
removeClient client room =
  modifyMVar room $ \r ->
    let r' = Room.removeClient client r in return (r', r')
