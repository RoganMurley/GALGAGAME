module Main.Types exposing (..)

import Room.Types as Room


type alias Model =
    { room : Room.Model
    , flags : Flags
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
