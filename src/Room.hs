module Room where

import Data.Maybe (maybeToList)
import Data.Text (Text)
import Network.WebSockets (Connection)

import Characters (initCharModel)
import GameState (GameState(..), Username, initState)
import Model (WhichPlayer(..))
import Util (Gen)


type RoomName = Text
type Client = (Username, Connection)
type Player = Maybe Client
type Spectators = [Client]


data Room = Room
  { room_pa    :: Player
  , room_pb    :: Player
  , room_specs :: Spectators
  , room_state :: GameState
  }


newRoom :: Gen -> Room
newRoom gen = Room
  { room_pa    = Nothing
  , room_pb    = Nothing
  , room_specs = []
  , room_state = initState gen
  }


getRoomGameState :: Room -> GameState
getRoomGameState Room{ room_state = state } = state


getRoomClients :: Room -> [Client]
getRoomClients room =
     (maybeToList (getPlayerClient PlayerA room))
  ++ (maybeToList (getPlayerClient PlayerB room))
  ++ getRoomSpecs room


getPlayerClient :: WhichPlayer -> Room -> Maybe Client
getPlayerClient PlayerA = room_pa
getPlayerClient PlayerB = room_pb


getRoomSpecs :: Room -> [Client]
getRoomSpecs = room_specs


clientExists :: Client -> Room -> Bool
clientExists client room =
  any ((== fst client) . fst) (getRoomClients room)


getSpeccingName :: Room -> Text
getSpeccingName room =
  case getPlayerClient PlayerA room of
    Just (name, _) ->
      name
    Nothing ->
      "nobody yet..."


addSpec :: Client -> Room -> Room
addSpec client room = room { room_specs = specs }
  where
    specs = client : getRoomSpecs room :: Spectators


addPlayer :: Client -> Room -> Maybe (Room, WhichPlayer)
addPlayer client room =
  (\which -> (ifFullInit . (setClient which client) $ room, which)) <$> (freeSlot room)
  where
    freeSlot :: Room -> Maybe WhichPlayer
    freeSlot Room{ room_pa = Nothing } = Just PlayerA
    freeSlot Room{ room_pb = Nothing } = Just PlayerB
    freeSlot _                         = Nothing


setClient :: WhichPlayer -> Client -> Room -> Room
setClient PlayerA client room = room { room_pa = Just client }
setClient PlayerB client room = room { room_pb = Just client }


ifFullInit :: Room -> Room
ifFullInit room =
  if roomFull room then
    case getRoomGameState room of
      Waiting gen ->
        room { room_state = Selecting initCharModel PlayerA gen }
      _ ->
        room
  else
    room


roomFull :: Room -> Bool
roomFull Room{ room_pa = (Just _), room_pb = (Just _) } = True
roomFull _                                              = False


roomEmpty :: Room -> Bool
roomEmpty Room{ room_pa = Nothing, room_pb = Nothing, room_specs = [] } = True
roomEmpty _                                                             = False


removeClientRoom :: Client -> Room -> Room
removeClientRoom client room@Room{ room_pa = pa, room_pb = pb, room_specs = specs } =
  room {
    room_pa    = newPlayer pa
  , room_pb    = newPlayer pb
  , room_specs = newSpecs
  }
  where
    newSpecs :: Spectators
    newSpecs = filter ((/= fst client) . fst) specs
    newPlayer :: Player -> Player
    newPlayer Nothing = Nothing
    newPlayer (Just c) = if fst c == fst client then Nothing else Just c
