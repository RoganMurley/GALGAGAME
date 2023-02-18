module Room where

import Client (Client)
import qualified Client
import Config (App)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import DeckBuilding (initDeckBuilding)
import GameCommand (nextSelectState)
import GameState (GameState (..), WaitType (..), initState)
import Outcome (Outcome (..))
import Player (WhichPlayer (..), other)
import Scenario (Scenario (..))
import User.User (GameUser (..), User (..), isCpu, isHuman, isSuperuser, usersToGameUsers)
import Util (Gen, randomChoice)

type Name = Text

data Player
  = ConnectedPlayer Client
  | DisconnectedPlayer User
  | NoPlayer
  deriving (Show)

type Spectators = [Client]

data Room = Room
  { room_pa :: Player,
    room_pb :: Player,
    room_specs :: Spectators,
    room_name :: Name,
    room_state :: GameState,
    room_scenario :: Scenario
  }
  deriving (Show)

new :: WaitType -> Gen -> Name -> Scenario -> Room
new wait gen name scenario =
  Room
    { room_pa = NoPlayer,
      room_pb = NoPlayer,
      room_specs = [],
      room_name = name,
      room_state = initState wait gen,
      room_scenario = scenario
    }

getName :: Room -> Name
getName Room {room_name = name} = name

getState :: Room -> GameState
getState Room {room_state = state} = state

setState :: Room -> GameState -> Room
setState room state = room {room_state = state}

getClients :: Room -> [Client]
getClients room =
  maybeToList (getPlayerClient PlayerA room)
    ++ maybeToList (getPlayerClient PlayerB room)
    ++ getSpecs room

getPlayerClient :: WhichPlayer -> Room -> Maybe Client
getPlayerClient PlayerA = clientFromPlayer . room_pa
getPlayerClient PlayerB = clientFromPlayer . room_pb

getSpecs :: Room -> [Client]
getSpecs = room_specs

getScenario :: Room -> Scenario
getScenario = room_scenario

addSpec :: Client -> Room -> Room
addSpec client room = room {room_specs = specs}
  where
    specs = client : getSpecs room :: Spectators

addPlayer :: Client -> Room -> Maybe (Room, [Outcome], WhichPlayer)
addPlayer client room =
  setup client room <$> freeSlot room
  where
    setup :: Client -> Room -> WhichPlayer -> (Room, [Outcome], WhichPlayer)
    setup c r w =
      let (newRoom, outcomes) = roomSetup $ setClient w c r
       in (newRoom, outcomes, w)
    freeSlot :: Room -> Maybe WhichPlayer
    freeSlot r
      | isSlotValid (room_pa r) client = Just PlayerA
      | isSlotValid (room_pb r) client = Just PlayerB
      | otherwise = Nothing
    isSlotValid :: Player -> Client -> Bool
    isSlotValid (DisconnectedPlayer user) c = Client.user c == user
    isSlotValid (ConnectedPlayer _) _ = False
    isSlotValid NoPlayer _ = True

setClient :: WhichPlayer -> Client -> Room -> Room
setClient PlayerA client room = room {room_pa = ConnectedPlayer client}
setClient PlayerB client room = room {room_pb = ConnectedPlayer client}

modScenario :: (Scenario -> Scenario) -> Room -> Room
modScenario f room = room {room_scenario = f (room_scenario room)}

roomSetup :: Room -> (Room, [Outcome])
roomSetup room =
  let (newRoom, outcomes) =
        case getState room of
          Waiting _ gen ->
            let scenario = getScenario room
                (userPa, userPb) = getUsers room
                superPa = maybe False isSuperuser userPa
                superPb = maybe False isSuperuser userPb
                deckBuildingModel =
                  initDeckBuilding
                    (superPa, superPb)
                    (scenario_characterPa scenario)
                    (scenario_characterPb scenario)
                turn =
                  case scenario_turn scenario of
                    Just t ->
                      t
                    Nothing ->
                      randomChoice gen [PlayerA, PlayerB]
                startProgram = scenario_prog scenario
                timeLimit = scenario_timeLimit scenario
                users = Room.getUsers room
                (state, newOutcomes) = nextSelectState deckBuildingModel turn (startProgram Nothing) gen users (posixSecondsToUTCTime 0) timeLimit
             in (room {room_state = state}, newOutcomes)
          _ ->
            (room, [])
   in (if full room then newRoom else room, outcomes)

full :: Room -> Bool
full Room {room_pa = (ConnectedPlayer _), room_pb = (ConnectedPlayer _)} = True
full _ = False

empty :: Room -> Bool
empty Room {room_pa = ConnectedPlayer _} = False
empty Room {room_pb = ConnectedPlayer _} = False
empty Room {room_specs = []} = True -- Neither player is connected, and no spectators
empty _ = False

removeClient :: Client -> Room -> Room
removeClient client room@Room {room_pa = pa, room_pb = pb, room_specs = specs} =
  room
    { room_pa = newPlayer pa,
      room_pb = newPlayer pb,
      room_specs = newSpecs
    }
  where
    newSpecs :: Spectators
    newSpecs = filter (/= client) specs
    newPlayer :: Player -> Player
    newPlayer (ConnectedPlayer c) =
      if c == client
        then DisconnectedPlayer (Client.user client)
        else ConnectedPlayer c
    newPlayer player = player

getUsers :: Room -> (Maybe User, Maybe User)
getUsers room = (userPa, userPb)
  where
    clientA = getPlayerClient PlayerA room :: Maybe Client
    clientB = getPlayerClient PlayerB room :: Maybe Client
    userPa = Client.user <$> clientA :: Maybe User
    userPb = Client.user <$> clientB :: Maybe User

getGameUsers :: Room -> App (Maybe GameUser, Maybe GameUser)
getGameUsers room =
  liftIO . atomically . usersToGameUsers $ getUsers room

-- Sending messages.
broadcast :: Text -> Room -> App ()
broadcast msg room = forM_ (Room.getClients room) (Client.send msg)

sendToPlayer :: WhichPlayer -> Text -> Room -> App ()
sendToPlayer which msg room =
  case Room.getPlayerClient which room of
    Just client ->
      Client.send msg client
    Nothing ->
      return ()

sendToSpecs :: Text -> Room -> App ()
sendToSpecs msg room = forM_ (Room.getSpecs room) (Client.send msg)

sendExcluding :: WhichPlayer -> Text -> Room -> App ()
sendExcluding which msg room = do
  sendToSpecs msg room
  sendToPlayer (other which) msg room

noHumans :: Room -> Bool
noHumans room =
  let (a, b) = getUsers room
   in not $ any isHuman (maybeToList a ++ maybeToList b)

noCpus :: Room -> Bool
noCpus room =
  let (a, b) = getUsers room
   in not $ any isCpu (maybeToList a ++ maybeToList b)

-- Player
clientFromPlayer :: Player -> Maybe Client
clientFromPlayer (ConnectedPlayer client) = Just client
clientFromPlayer _ = Nothing
