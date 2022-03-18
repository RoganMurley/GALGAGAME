module Lobby.Messages exposing (Msg(..))


type Msg
    = JoinRoom
    | JoinRoomErr String
    | SetRoom String
