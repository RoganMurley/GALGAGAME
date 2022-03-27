module Lobby.Messages exposing (Msg(..))


type Msg
    = JoinRoom Bool
    | JoinRoomErr String
    | SetRoom String
