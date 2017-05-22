module Chat.Types exposing (Model)

import Mouse exposing (Position)
import Drag.Types exposing (Drag)


type alias Model =
    { input : String
    , messages : List String
    , pos : Position
    , drag : Maybe Drag
    }
