module Presence.Messages exposing (..)

import Http
import Presence.Types exposing (User)


type Msg
    = Load
    | LoadCallback (Result Http.Error (List User))
    | Challenge Int
    | ChallengeCPU
