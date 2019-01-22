module PlayState.Messages exposing (Msg(..), PlayingOnly(..), TurnOnly(..))


type Msg
    = HoverOutcome (Maybe Int)
    | DamageOutcome ( Int, Int )
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
