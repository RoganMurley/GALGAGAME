module PlayState.Messages exposing (Msg(..), PlayingOnly(..), TurnOnly(..))


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
