module Replay.Messages exposing (Msg(..))

import Http
import Mouse
import Replay.Types exposing (Replay)


type Msg
    = Load String Float
    | LoadCallback Float (Result Http.Error Replay)
    | SetPlaying Bool
    | SpeedUp
    | SlowDown
    | NoOp
    | DragStart Mouse.Position
    | SetReverse Bool
