module Leaderboard.Messages exposing (..)

import Http
import Leaderboard.Types exposing (Entry)


type Msg
    = Load
    | LoadCallback (Result Http.Error (List Entry))
