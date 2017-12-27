module Main.Types exposing (..)

import Room.Types as Room
import Settings.Types as Settings


type alias Model =
    { room : Room.Model
    , flags : Flags
    , settings : Settings.Model
    }


type alias Flags =
    { hostname : String
    , httpPort : String
    , seed : Seed
    , time : Float
    , dimensions : ( Int, Int )
    }


type alias Seed =
    Int
