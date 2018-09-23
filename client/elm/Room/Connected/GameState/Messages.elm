module GameState.Messages exposing (..)

import Mouse exposing (Position)
import CharacterSelect.Messages as CharacterSelect


type Msg
    = HoverSelf (Maybe Int)
    | HoverOutcome (Maybe Int)
    | ResolveOutcome String
    | SelectingMsg CharacterSelect.Msg
    | Sync String
    | PlayingOnly PlayingOnly
    | ReplaySaved String
    | GotoReplay String
    | Mouse Position


type PlayingOnly
    = Rematch
    | TurnOnly TurnOnly
    | HoverCard (Maybe Int)


type TurnOnly
    = EndTurn
    | PlayCard Int
