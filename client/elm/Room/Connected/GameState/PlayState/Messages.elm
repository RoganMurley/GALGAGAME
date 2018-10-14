module PlayState.Messages exposing (..)


type Msg
    = HoverSelf (Maybe Int)
    | HoverOutcome (Maybe Int)
    | PlayingOnly PlayingOnly
    | ReplaySaved String
    | GotoReplay String


type PlayingOnly
    = Rematch
    | TurnOnly TurnOnly
    | HoverCard (Maybe Int)


type TurnOnly
    = EndTurn
    | PlayCard Int
