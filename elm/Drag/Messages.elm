module Drag.Messages exposing (Msg(..))

import Drag.Types exposing (Position)


type Msg
    = Start Position
    | At Position
    | End Position
