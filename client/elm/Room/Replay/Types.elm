module Replay.Types exposing (..)

import GameState.Types exposing (PlayState)


type alias Model =
    { replay : Maybe Replay
    }


type alias Replay =
    { state : PlayState
    , usernamePa : String
    , usernamePb : String
    }
