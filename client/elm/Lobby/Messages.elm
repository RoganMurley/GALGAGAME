module Lobby.Messages exposing (Msg(..))

import Mode exposing (Mode)


type Msg
    = JoinRoom Mode
    | JoinRoomErr String
