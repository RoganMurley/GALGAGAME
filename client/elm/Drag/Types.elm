module Drag.Types exposing (Drag, Draggable, Position)

import Mouse


type alias Position =
    Mouse.Position


type alias Drag =
    { start : Position
    , current : Position
    }


type alias Draggable a =
    { a
        | pos : Position
        , drag : Maybe Drag
    }
