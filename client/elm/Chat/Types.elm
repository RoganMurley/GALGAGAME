module Chat.Types exposing (ChatMessage, Model)

import Mouse


type alias Model =
    { input : String
    , messages : List ChatMessage
    , visible : Bool
    , notify : Bool
    , pos : Mouse.Position
    , drag : Maybe Mouse.Position
    }


type alias ChatMessage =
    { username : String
    , message : String
    }
