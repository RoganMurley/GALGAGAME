module Server where

import Prelude hiding (lookup, putStrLn)

import Control.Concurrent (MVar, newMVar, modifyMVar)
import Data.Map.Strict (Map, delete, empty, insert, keys, lookup)
import Data.Monoid ((<>))
import Data.Text.IO (putStrLn)
import Data.String.Conversions (cs)

import Util (getGen)

import qualified Room
import Room (Room)


newtype State = State (Map Room.Name (MVar Room))


instance Show State where
  show (State rooms) = show . keys $ rooms


initState :: State
initState = State empty


getRoom :: Room.Name -> MVar State -> IO (MVar Room)
getRoom name state =
  modifyMVar state $ \(State s) ->
    do
      putStrLn . ("Rooms: " <>) . cs . show . State $ s
      case lookup name s of
        Just room ->
          return (State s, room)
        Nothing ->
          do
            gen <- getGen
            r <- newMVar (Room.new gen)
            return (State (insert name r s), r)


deleteRoom :: Room.Name -> MVar State -> IO (State)
deleteRoom name state =
  modifyMVar state $ \(State s) ->
    let s' = State (delete name s) in return (s', s')
