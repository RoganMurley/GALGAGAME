module Chat.Types exposing (Model)

import Mouse


type alias Model =
    { input : String
    , messages : List String
    , visible : Bool
    , notify : Bool
    , pos : Mouse.Position
    , drag : Maybe Mouse.Position
    }
