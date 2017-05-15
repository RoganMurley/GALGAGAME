module Lobby.Messages exposing (Msg(..))

import Main.Types exposing (Mode)


type Msg
    = NameInput String
    | JoinRoom Mode
    | JoinRoomErr String
