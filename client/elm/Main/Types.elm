module Main.Types exposing (..)

import Connected.Types as Connected
import Lab.Types as Lab
import Lobby.Types as Lobby


type alias Model =
    { room : RoomModel
    , hostname : String
    , httpPort : String
    , frameTime : Float
    , seed : Seed
    , windowDimensions : ( Int, Int )
    }


type alias Seed =
    Int


type RoomModel
    = MainMenu
    | Connecting Lobby.Model
    | Connected Connected.Model
    | Lab Lab.Model


type alias Flags =
    { hostname : String
    , httpPort : String
    , seed : Seed
    , windowDimensions : ( Int, Int )
    }
