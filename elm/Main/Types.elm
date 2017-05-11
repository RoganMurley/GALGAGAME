module Main.Types exposing (..)

import Chat.Types as Chat
import GameState.Types as GameState exposing (GameState(..))


type alias Model =
    { room : RoomModel
    , hostname : String
    , httpPort : String
    , frameTime : Float
    , windowDimensions : ( Int, Int )
    }


type alias Seed =
    Int


type RoomModel
    = MainMenu Seed
    | Connecting ConnectingModel
    | Connected ConnectedModel


type GameType
    = CustomGame
    | ComputerGame


type alias ConnectingModel =
    { roomID : String
    , name : String
    , error : String
    , valid : Bool
    , gameType : GameType
    }


type alias ConnectedModel =
    { chat : Chat.Model
    , game : GameState.GameState
    , mode : Mode
    , roomID : String
    }


type alias Flags =
    { hostname : String
    , httpPort : String
    , play : Maybe String
    , seed : Seed
    , windowDimensions : ( Int, Int )
    }


type Mode
    = Spectating
    | Playing
