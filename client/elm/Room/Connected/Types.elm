module Connected.Types exposing (Model)

import GameState.Types exposing (GameState)
import GameType exposing (GameType)
import Mode exposing (Mode)


type alias Model =
    { game : GameState
    , mode : Mode
    , gameType : GameType
    , roomID : String
    , players : ( Maybe String, Maybe String )
    , tick : Float
    }
