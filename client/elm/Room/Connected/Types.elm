module Connected.Types exposing (Model)

import GameState.Types exposing (GameState)
import Mode exposing (Mode)


type alias Model =
    { game : GameState
    , mode : Mode
    , roomID : String
    , players : ( Maybe String, Maybe String )
    , tick : Float
    }
