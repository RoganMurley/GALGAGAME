module Lobby.Types exposing (GameType(..), Model)


type alias Model =
    { roomID : String
    , error : String
    , gameType : GameType
    }


type GameType
    = CustomGame
    | ComputerGame
    | QuickplayGame
