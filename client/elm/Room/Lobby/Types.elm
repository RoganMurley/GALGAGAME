module Lobby.Types exposing (..)

import Mode exposing (Mode(..))


type alias Model =
    { roomID : String
    , error : String
    , gameType : GameType
    , mode : Mode
    , login : LoginState
    }


type GameType
    = CustomGame
    | ComputerGame
    | QuickplayGame


type LoginState
    = ChooseLoginOption
    | Login
