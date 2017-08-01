module GameState.Messages exposing (Msg(..))

import CharacterSelect.Messages as CharacterSelect


type Msg
    = HoverSelf (Maybe Int)
    | HoverOutcome (Maybe Int)
    | ResolveOutcome String
    | SelectingMsg CharacterSelect.Msg
    | Sync String
