module PlayState.Messages exposing (Msg(..), PlayingOnly(..), TurnOnly(..))

import Game.Types exposing (HoverOther, HoverSelf)


type Msg
    = HoverOtherOutcome HoverOther
    | DamageOutcome ( Int, Int )
    | PlayingOnly PlayingOnly
    | ReplaySaved String
    | GotoReplay String


type PlayingOnly
    = Rematch
    | TurnOnly TurnOnly
    | HoverCard HoverSelf


type TurnOnly
    = EndTurn
    | PlayCard Int
