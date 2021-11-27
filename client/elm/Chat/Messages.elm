module Chat.Messages exposing (Msg(..))


type Msg
    = RecvMessage String
    | SetVisibility Bool
