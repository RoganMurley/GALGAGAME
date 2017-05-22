module Chat.Messages exposing (Msg(..))


type Msg
    = New String
    | Send
    | Input String
