module Connected.Types exposing (..)

import GameState.Types as GameState exposing (GameState)
import Mode exposing (Mode)
import Settings.Types as Settings


type alias Model =
    { game : GameState.GameState
    , settings : Settings.Model
    , mode : Mode
    , roomID : String
    , players : ( Maybe String, Maybe String )
    }
