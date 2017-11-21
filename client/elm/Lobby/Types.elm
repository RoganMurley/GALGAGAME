module Lobby.Types exposing (GameType(..), Model)

import Mode exposing (Mode(..))


type alias Model =
    { roomID : String
    , error : String
    , gameType : GameType
    , mode : Mode
    }


type GameType
    = CustomGame
    | ComputerGame
    | QuickplayGame
