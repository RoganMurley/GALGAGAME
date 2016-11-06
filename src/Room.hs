{-# LANGUAGE OverloadedStrings #-}
module Room where

import Data.Maybe (maybeToList)
import Data.Text (Text)
import Network.WebSockets (Connection)
import System.Random (StdGen)

import GameState (initModel, GameState(..), WhichPlayer(..))


--TYPES
type Username = Text
type RoomName = Text

type Client = (Username, Connection)

type Player = Maybe Client
type Spectators = [Client]

data Room = Room Player Player Spectators GameState


-- INITIAL
newRoom :: StdGen -> Room
newRoom gen = Room Nothing Nothing [] (Waiting gen)


-- GETTERS
getRoomGameState :: Room -> GameState
getRoomGameState (Room _ _ _ state) = state

getRoomClients :: Room -> [Client]
getRoomClients (Room pa pb specs _) =
  (maybeToList pa) ++ (maybeToList pb) ++ specs

getPlayerClient :: WhichPlayer -> Room -> Maybe Client
getPlayerClient PlayerA (Room pa _ _ _) = pa
getPlayerClient PlayerB (Room _ pb _ _) = pb

getRoomSpecs :: Room -> [Client]
getRoomSpecs (Room _ _ specs _) = specs

clientExists :: Client -> Room -> Bool
clientExists client room = any ((== fst client) . fst) (getRoomClients room)


-- ADD CLIENTS.
addSpec :: Client -> Room -> Room
addSpec client (Room pa pb specs state) = Room pa pb (client:specs) state

addPlayer :: Client -> Room -> (Room, WhichPlayer)
addPlayer client (Room Nothing pb specs state) = (Room (Just client) pb specs state, PlayerA)
addPlayer client (Room pa Nothing specs state) = (Room pa (Just client) specs state, PlayerB)
addPlayer client (Room (Just a) (Just b) specs state) = (Room (Just a) (Just b) specs state, PlayerA) -- FIX THIS: PLAYERA -> NOTHING OR SOMETHING

roomFull :: Room -> Bool
roomFull (Room (Just _) (Just _) _ _) = True
roomFull _ = False

roomEmpty :: Room -> Bool
roomEmpty (Room Nothing Nothing [] _) = True
roomEmpty _ = False


-- REMOVE CLIENTS.
removeClientRoom :: Client -> Room -> Room
removeClientRoom client (Room pa pb specs state) =
  Room (newPlayer pa) (newPlayer pb) newSpecs state
  where
  newSpecs :: Spectators
  newSpecs = filter ((/= fst client) . fst) specs
  newPlayer :: Player -> Player
  newPlayer Nothing = Nothing
  newPlayer (Just c) = if fst c == fst client then Nothing else Just c
