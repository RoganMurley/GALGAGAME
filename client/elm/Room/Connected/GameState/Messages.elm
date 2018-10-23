module GameState.Messages exposing (Msg(..))

import CharacterSelect.Messages as CharacterSelect
import Mouse exposing (Position)
import PlayState.Messages as PlayState


type Msg
    = Mouse Position
    | MouseClick Position
    | PlayStateMsg PlayState.Msg
    | ResolveOutcome String
    | SelectingMsg CharacterSelect.Msg
    | Sync String
