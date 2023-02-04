module Replay.Types exposing (Model, Replay)

import Mouse
import PlayState.Types exposing (PlayState)


type alias Model =
    { replay : Maybe Replay
    , started : Bool
    , error : String
    , playing : Bool
    , speed : Float
    , frame : Float
    , pos : Mouse.Position
    , drag : Maybe Mouse.Position
    , id : String
    , reverse : Bool
    , fastforward : Bool
    }


type alias Replay =
    { state : PlayState
    , usernamePa : String
    , usernamePb : String
    , tick : Float
    }
