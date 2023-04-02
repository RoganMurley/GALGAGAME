module Presence.Messages exposing (..)

import Http


type Msg
    = Load
    | LoadCallback (Result Http.Error (List String))
    | Challenge String
