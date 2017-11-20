module GameState.Messages exposing (..)

import CharacterSelect.Messages as CharacterSelect


type Msg
    = HoverSelf (Maybe Int)
    | HoverOutcome (Maybe Int)
    | ResolveOutcome String
    | SelectingMsg CharacterSelect.Msg
    | Sync String
    | Shake Float
    | PlayingOnly PlayingOnly


type PlayingOnly
    = Rematch
    | TurnOnly TurnOnly


type TurnOnly
    = EndTurn
    | PlayCard Int
