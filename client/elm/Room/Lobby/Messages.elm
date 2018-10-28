module Lobby.Messages exposing (Msg(..))


type Msg
    = JoinRoom
    | JoinRoomErr String
    | GotoLogin
    | GotoSignup
