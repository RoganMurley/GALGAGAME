
module Room where

import Data.Maybe (maybeToList)
import Data.Text (Text)
import Network.WebSockets (Connection)

import Characters (initCharModel)
import GameState (GameState(..), Username)
import Model (WhichPlayer(..))
import Util (Gen)


type RoomName = Text

type Client = (Username, Connection)

type Player = Maybe Client
type Spectators = [Client]

data Room = Room Player Player Spectators GameState


newRoom :: Gen -> Room
newRoom gen = Room Nothing Nothing [] (Waiting Nothing Nothing gen)


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

getSpeccingName :: Room -> Text
getSpeccingName (Room (Just (name, _))_ _ _) = name
getSpeccingName (Room Nothing _ _ _) = "nobody yet..."


addSpec :: Client -> Room -> Room
addSpec client (Room pa pb specs state) = Room pa pb (client:specs) state

addPlayer :: Client -> Room -> Maybe (Room, WhichPlayer)
addPlayer client (Room Nothing pb specs state) = Just (ifFullInit $ Room (Just client) pb specs state, PlayerA)
addPlayer client (Room pa Nothing specs state) = Just (ifFullInit $ Room pa (Just client) specs state, PlayerB)
addPlayer _      (Room (Just _) (Just _) _ _)  = Nothing

ifFullInit :: Room -> Room
ifFullInit room
  | roomFull room =
    case room of
      Room pa pb specs (Waiting _ _ std) ->
        (Room pa pb specs) $ Selecting initCharModel std
      _ ->
        room
  | otherwise = room

roomFull :: Room -> Bool
roomFull (Room (Just _) (Just _) _ _) = True
roomFull _ = False

roomEmpty :: Room -> Bool
roomEmpty (Room Nothing Nothing [] _) = True
roomEmpty _ = False


removeClientRoom :: Client -> Room -> Room
removeClientRoom client (Room pa pb specs state) =
  Room (newPlayer pa) (newPlayer pb) newSpecs state
  where
  newSpecs :: Spectators
  newSpecs = filter ((/= fst client) . fst) specs
  newPlayer :: Player -> Player
  newPlayer Nothing = Nothing
  newPlayer (Just c) = if fst c == fst client then Nothing else Just c
