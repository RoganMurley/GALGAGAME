module Entrypoint.Messages exposing (..)

import Entrypoint.Types exposing (User)
import Http


type Msg
    = Load
    | LoadCallback (Result Http.Error (List User))
    | Challenge Int
    | Quickplay
