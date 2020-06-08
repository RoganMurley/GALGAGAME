module Main.Types exposing (Flags, InputFlags, Model, Seed)

import Browser.Navigation
import Math.Vector2 exposing (Vec2)
import Room.Types as Room
import Settings.Types as Settings
import Texture.Types as Texture


type alias Model =
    { room : Room.Model
    , flags : Flags
    , settings : Settings.Model
    , textures : Texture.Model
    }


type alias InputFlags =
    { hostname : String
    , httpPort : String
    , seed : Seed
    , time : Float
    , dimensions : ( Int, Int )
    , username : Maybe String
    , pixelRatio : Float
    , initialVolume : Int
    }


type alias Flags =
    { hostname : String
    , httpPort : String
    , seed : Seed
    , time : Float
    , dimensions : ( Int, Int )
    , username : Maybe String
    , mouse : Maybe Vec2
    , key : Browser.Navigation.Key
    , pixelRatio : Float
    }


type alias Seed =
    Int
