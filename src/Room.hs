module Room where

import Data.Maybe (maybeToList)
import Data.Text (Text)
import Network.WebSockets (Connection, sendTextData)

import Characters (initCharModel)
import GameState (GameState(..), Username, initState)
import Model (WhichPlayer(..))
import Util (Gen)


type RoomName = Text
data ClientConnection = PlayerConnection Connection | ComputerConnection
type Client = (Username, ClientConnection)
type Player = Maybe Client
type Spectators = [Client]


data Room = Room
  { room_pa    :: Player
  , room_pb    :: Player
  , room_specs :: Spectators
  , room_state :: GameState
  }


new :: Gen -> Room
new gen = Room
  { room_pa    = Nothing
  , room_pb    = Nothing
  , room_specs = []
  , room_state = initState gen
  }


getState :: Room -> GameState
getState Room{ room_state = state } = state


setState :: Room -> GameState -> Room
setState room state = room { room_state = state }


getClients :: Room -> [Client]
getClients room =
     (maybeToList (getPlayerClient PlayerA room))
  ++ (maybeToList (getPlayerClient PlayerB room))
  ++ getSpecs room


getPlayerClient :: WhichPlayer -> Room -> Maybe Client
getPlayerClient PlayerA = room_pa
getPlayerClient PlayerB = room_pb


getSpecs :: Room -> [Client]
getSpecs = room_specs


clientExists :: Client -> Room -> Bool
clientExists client room =
  any ((== fst client) . fst) (getClients room)


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
    specs = client : getSpecs room :: Spectators


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
  if full room then
    case getState room of
      Waiting gen ->
        room { room_state = Selecting initCharModel PlayerA gen }
      _ ->
        room
  else
    room


full :: Room -> Bool
full Room{ room_pa = (Just _), room_pb = (Just _) } = True
full _                                              = False


empty :: Room -> Bool
empty Room{ room_pa = Nothing, room_pb = Nothing, room_specs = [] } = True
empty _                                                             = False


removeClient :: Client -> Room -> Room
removeClient client room@Room{ room_pa = pa, room_pb = pb, room_specs = specs } =
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


sendToClient :: Text -> Client -> IO ()
sendToClient message (_, PlayerConnection conn) =
  sendTextData conn message
sendToClient _ _ =
  return ()
