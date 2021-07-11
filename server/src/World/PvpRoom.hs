module World.PvpRoom where

import GameState (WaitType(..), initState)
import Util (Gen)

import qualified Room
import Room (Room(..))


newPvpRoom :: Player -> Player -> Text -> Gen -> Room
newPvpRoom pa pb name gen =
  Room
    { room_pa = pa
    , room_pb = pb
    , room_specs = []
    , room_name = name
    , room_state    = initState WaitWorldPvp gen
    , room_scenario = initState WaitWorldPvp gen
    }
