module Connected.Types exposing (..)

import GameState.Types as GameState exposing (GameState)
import Mode exposing (Mode)


type alias Model =
    { game : GameState.GameState
    , mode : Mode
    , roomID : String
    , players : ( Maybe String, Maybe String )
    }
