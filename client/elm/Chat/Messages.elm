module Chat.Messages exposing (Msg(..))

import Mouse


type Msg
    = RecvMessage String
    | SendMessage String
    | ToggleVisibility
    | SetInput String
    | DragStart Mouse.Position
