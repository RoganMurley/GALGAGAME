module Lobby.Messages exposing (Msg(..))

import Main.Types exposing (Mode)


type Msg
    = NameInput String
    | JoinRoomErr String
