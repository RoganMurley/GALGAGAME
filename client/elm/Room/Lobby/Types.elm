module Lobby.Types exposing (LoginState(..), Model)

import GameType exposing (GameType)
import Mode exposing (Mode(..))


type alias Model =
    { roomID : String
    , error : String
    , gameType : GameType
    , mode : Mode
    , login : LoginState
    }


type LoginState
    = ChooseLoginOption
    | Login
