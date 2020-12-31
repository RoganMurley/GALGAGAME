module Room where

import Config (App)
import Control.Monad (forM_)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import DeckBuilding (initDeckBuilding)
import GameCommand (nextSelectState)
import GameState (GameState(..), WaitType(..), initState)
import Outcome (Outcome(..))
import Player (WhichPlayer(..), other)
import Scenario (Scenario(..))
import Util (Gen)
import User (User)

import qualified Client
import Client (Client)


type Name       = Text
type Player     = Maybe Client
type Spectators = [Client]


data Room = Room
  { room_pa       :: Player
  , room_pb       :: Player
  , room_specs    :: Spectators
  , room_name     :: Name
  , room_state    :: GameState
  , room_scenario :: Scenario
  } deriving (Show)


new :: WaitType -> Gen -> Name -> Scenario -> Room
new wait gen name scenario =
  Room
    { room_pa = Nothing
    , room_pb = Nothing
    , room_specs = []
    , room_name = name
    , room_state = initState wait gen
    , room_scenario = scenario
    }


getName :: Room -> Name
getName Room{ room_name = name } = name


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


getScenario :: Room -> Scenario
getScenario = room_scenario


addSpec :: Client -> Room -> Room
addSpec client room = room { room_specs = specs }
  where
    specs = client : getSpecs room :: Spectators


addPlayer :: Client -> Room -> Maybe (Room, [Outcome],  WhichPlayer)
addPlayer client room =
  setup client room <$> freeSlot room
  where
    setup :: Client -> Room -> WhichPlayer -> (Room, [Outcome], WhichPlayer)
    setup c r w =
      let
        (newRoom, outcomes) = roomSetup $ setClient w c r
      in
        (newRoom, outcomes, w)
    freeSlot :: Room -> Maybe WhichPlayer
    freeSlot Room{ room_pa = Nothing } = Just PlayerA
    freeSlot Room{ room_pb = Nothing } = Just PlayerB
    freeSlot _                         = Nothing


setClient :: WhichPlayer -> Client -> Room -> Room
setClient PlayerA client room = room { room_pa = Just client }
setClient PlayerB client room = room { room_pb = Just client }


roomSetup :: Room -> (Room, [Outcome])
roomSetup room =
  let
    (newRoom, outcomes) =
      case getState room of
        Waiting _ gen ->
          let
            scenario = getScenario room
            deckBuildingModel = initDeckBuilding (scenario_characterPa scenario) (scenario_characterPb scenario)
            turn = scenario_turn scenario
            startProgram = scenario_prog scenario
            users = Room.getUsers room
            (state, newOutcomes) = nextSelectState deckBuildingModel turn startProgram gen users
          in
            (room { room_state = state }, newOutcomes)
        _ ->
          (room, [])
  in
    (if full room then newRoom else room, outcomes)


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
    newSpecs = filter (/= client) specs
    newPlayer :: Player -> Player
    newPlayer Nothing = Nothing
    newPlayer (Just c) =
      if c == client
        then Nothing
        else Just c


getUsers :: Room -> (Maybe User, Maybe User)
getUsers room = (userPa, userPb)
  where
    clientA = getPlayerClient PlayerA room :: Maybe Client
    clientB = getPlayerClient PlayerB room :: Maybe Client
    userPa = Client.user <$> clientA :: Maybe User
    userPb = Client.user <$> clientB :: Maybe User


players :: Room -> (Player, Player)
players Room{ room_pa, room_pb } = (room_pa, room_pb)


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
