module Main.Types exposing (..)

import Room.Types as Room
import Settings.Types as Settings
import Texture.Types as Texture


type alias Model =
    { room : Room.Model
    , flags : Flags
    , settings : Settings.Model
    , textures : Texture.Model
    }


type alias Flags =
    { hostname : String
    , httpPort : String
    , seed : Seed
    , time : Float
    , dimensions : ( Int, Int )
    , username : Maybe String
    }


type alias Seed =
    Int
