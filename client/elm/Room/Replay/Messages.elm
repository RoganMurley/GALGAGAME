module Replay.Messages exposing (Msg(..))

import Http
import Replay.Types exposing (Replay)


type Msg
    = Load String
    | LoadCallback (Result Http.Error Replay)
