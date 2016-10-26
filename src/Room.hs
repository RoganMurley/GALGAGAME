{-# LANGUAGE OverloadedStrings #-}
module Room where

import Data.Maybe (maybeToList)
import Data.Text (Text)
import Network.WebSockets (Connection)
import System.Random (StdGen)

import GameState (initModel, Model, WhichPlayer(..))


--TYPES
type Username = Text
type RoomName = Text

type Client = (Username, Connection)

type Player = Maybe Client
type Spectators = [Client]

data Room = Room Player Player Spectators Model


-- INITIAL
newRoom :: StdGen -> Room
newRoom gen = Room Nothing Nothing [] (initModel gen)


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

clientExists :: Client -> Room -> Bool
clientExists client room = any ((== fst client) . fst) (getRoomClients room)


-- ADD CLIENTS.
addSpec :: Client -> Room -> Room
addSpec client (Room pa pb specs model) = Room pa pb (client:specs) model

addPlayer :: Client -> Room -> (Room, WhichPlayer)
addPlayer client (Room Nothing pb specs model) = (Room (Just client) pb specs model, PlayerA)
addPlayer client (Room pa Nothing specs model) = (Room pa (Just client) specs model, PlayerB)
addPlayer client (Room (Just a) (Just b) specs model) = (Room (Just a) (Just b) specs model, PlayerA) -- FIX THIS: PLAYERA -> NOTHING OR SOMETHING

roomFull :: Room -> Bool
roomFull (Room (Just _) (Just _) _ _) = True
roomFull _ = False

roomEmpty :: Room -> Bool
roomEmpty (Room Nothing Nothing [] _) = True
roomEmpty _ = False


-- REMOVE CLIENTS.
removeClientRoom :: Client -> Room -> Room
removeClientRoom client (Room pa pb specs model) =
  Room (newPlayer pa) (newPlayer pb) newSpecs model
  where
  newSpecs :: Spectators
  newSpecs = filter ((/= fst client) . fst) specs
  newPlayer :: Player -> Player
  newPlayer Nothing = Nothing
  newPlayer (Just c) = if fst c == fst client then Nothing else Just c
