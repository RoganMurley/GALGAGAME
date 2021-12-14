module Connected.Types exposing (Model, Players)

import Chat.Types as Chat
import GameState.Types exposing (GameState)
import GameType exposing (GameType)
import Mode exposing (Mode)


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


type alias Players =
    { pa : Maybe String
    , pb : Maybe String
    }
