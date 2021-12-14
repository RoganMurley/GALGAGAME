module Connected.Types exposing (Model)

import Chat.Types as Chat
import GameState.Types exposing (GameState)
import GameType exposing (GameType)
import Mode exposing (Mode)
import Players exposing (Players)


type alias Model =
    { game : GameState
    , mode : Mode
    , gameType : GameType
    , roomID : String
    , players : Players
    , tick : Float
    , errored : Bool
    , chat : Chat.Model
    , heartbeatTick : Float
    , connectionLost : Bool
    }
