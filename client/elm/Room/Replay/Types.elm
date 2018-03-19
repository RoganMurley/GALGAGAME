module Replay.Types exposing (Model)

import GameState.Types exposing (PlayState)


type alias Model =
    { replay : Maybe PlayState
    }
