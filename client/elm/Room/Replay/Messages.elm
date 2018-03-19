module Replay.Messages exposing (Msg(..))

import GameState.Types exposing (PlayState)


type Msg
    = SetReplay PlayState
