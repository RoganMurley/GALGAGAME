module Connected.Types exposing (..)

import GameState.Types exposing (GameState)
import Mode exposing (Mode)


type alias Model =
    { game : GameState
    , mode : Mode
    , roomID : String
    , players : ( Maybe String, Maybe String )
    }
