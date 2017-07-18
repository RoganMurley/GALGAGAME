module Main.Types exposing (..)

import Chat.Types as Chat
import GameModal.Types as GameModal
import GameState.Types as GameState exposing (GameState(..))
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
    | Connected ConnectedModel


type alias ConnectedModel =
    { chat : Chat.Model
    , game : GameState.GameState
    , modal : GameModal.Model
    , mode : Mode
    , roomID : String
    }


type alias Flags =
    { hostname : String
    , httpPort : String
    , seed : Seed
    , windowDimensions : ( Int, Int )
    }


type Mode
    = Spectating
    | Playing
