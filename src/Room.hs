{-# LANGUAGE OverloadedStrings #-}
module Room where

import Data.Maybe (maybeToList)
import Data.Text (Text)
import Network.WebSockets (Connection)

import GameState (initModel, Model, WhichPlayer(..))


--TYPES
type Username = Text
type RoomName = Text

type Client = (Username, Connection)

type Player = Maybe Client
type Spectators = [Client]

data Room = Room Player Player Spectators Model


-- INITIAL
newRoom :: Room
newRoom = Room Nothing Nothing [] initModel


-- GETTERS
getRoomModel :: Room -> Model
getRoomModel (Room _ _ _ model) = model

getRoomClients :: Room -> [Client]
getRoomClients (Room pa pb specs _) =
  (maybeToList pa) ++ (maybeToList pb) ++ specs

getPlayerClient :: WhichPlayer -> Room -> Maybe Client
getPlayerClient PlayerA (Room pa _ _ _) = pa
getPlayerClient PlayerB (Room _ pb _ _) = pb

getRoomSpecs :: Room -> [Client]
getRoomSpecs (Room _ _ specs _) = specs


-- ADD CLIENTS.
addSpec :: Client -> Room -> Room
addSpec client (Room pa pb specs count) = Room pa pb (client:specs) count

addPlayer :: Client -> Room -> (Room, WhichPlayer)
addPlayer client (Room Nothing pb specs count) = (Room (Just client) pb specs count, PlayerA)
addPlayer client (Room pa Nothing specs count) = (Room pa (Just client) specs count, PlayerB)
addPlayer client (Room (Just a) (Just b) specs count) = (Room (Just a) (Just b) specs count, PlayerA) -- FIX THIS: PLAYERA -> NOTHING OR SOMETHING

roomFull :: Room -> Bool
roomFull (Room (Just _) (Just _) _ _) = True
roomFull _ = False


-- REMOVE CLIENTS.
removeClientRoom :: Client -> Room -> Room
removeClientRoom client (Room pa pb specs count) =
  Room (newPlayer pa) (newPlayer pb) newSpecs count
  where
  newSpecs :: Spectators
  newSpecs = filter ((/= fst client) . fst) specs
  newPlayer :: Player -> Player
  newPlayer Nothing = Nothing
  newPlayer (Just c) = if fst c == fst client then Nothing else Just c
