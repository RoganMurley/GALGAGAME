module Lobby.Messages exposing (Msg(..))

import Main.Types exposing (Mode)


type Msg
    = JoinRoom Mode
    | JoinRoomErr String
