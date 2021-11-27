module Chat.Messages exposing (Msg(..))


type Msg
    = RecvMessage String
    | SendMessage String
    | ToggleVisibility
    | SetInput String
