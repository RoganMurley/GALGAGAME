module GameState.Messages exposing (..)

import CharacterSelect.Messages as CharacterSelect


type Msg
    = HoverSelf (Maybe Int)
    | HoverOutcome (Maybe Int)
    | ResolveOutcome String
    | SelectingMsg CharacterSelect.Msg
    | Sync String
    | Shake Float
    | TurnAction TurnAction


type TurnAction
    = EndTurn
    | PlayCard Int
