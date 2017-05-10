module GameState.Messages exposing (Msg(..))

import CharacterSelect.Messages as CharacterSelect


type Msg
    = HoverOutcome (Maybe Int)
    | ResolveOutcome String
    | SelectingMsg CharacterSelect.Msg
    | Sync String
