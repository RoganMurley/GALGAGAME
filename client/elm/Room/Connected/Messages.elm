module Connected.Messages exposing (Msg(..))

import Chat.Messages as Chat
import GameState.Messages as GameState


type Msg
    = Concede
    | ChatMsg Chat.Msg
    | GameStateMsg GameState.Msg
    | Reconnect
