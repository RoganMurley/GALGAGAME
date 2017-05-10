module Chat.Types exposing (Model)

import Mouse exposing (Position)
import Drag exposing (Drag)


type alias Model =
    { input : String
    , messages : List String
    , pos : Position
    , drag : Maybe Drag
    }
