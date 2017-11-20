module Lobby.Messages exposing (Msg(..))

import Connected.Types exposing (Mode)


type Msg
    = JoinRoom Mode
    | JoinRoomErr String
