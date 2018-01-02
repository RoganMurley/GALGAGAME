module Connected.Messages exposing (Msg(..))

import GameState.Messages as GameState


type Msg
    = Concede
    | GameStateMsg GameState.Msg
