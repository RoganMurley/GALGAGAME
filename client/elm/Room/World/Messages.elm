module World.Messages exposing (Msg(..))

import World.Types exposing (World)


type Msg
    = JoinWorld
    | LoadWorld World
