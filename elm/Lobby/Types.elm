module Lobby.Types exposing (GameType(..), Model)


type alias Model =
    { roomID : String
    , name : String
    , error : String
    , valid : Bool
    , gameType : GameType
    }


type GameType
    = CustomGame
    | ComputerGame
