module PlayState.Messages exposing (Msg(..), PlayingOnly(..), TurnOnly(..))

import Card.Types exposing (Card)
import Collision
import Hover exposing (HoverOther, HoverSelf)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Stats exposing (StatChange)


type Msg
    = HoverOtherOutcome HoverOther
    | DamageOutcome ( Int, Int )
    | PlayingOnly PlayingOnly
    | ReplaySaved String
    | GotoReplay String
    | GotoComputerGame
    | StatChange StatChange
    | ClickFeedback Vec2


type PlayingOnly
    = Rematch
    | TurnOnly TurnOnly
    | HoverCard HoverSelf
    | IllegalPass


type TurnOnly
    = EndTurn
    | PlayCard Card Int Vec3
    | HoldCard Card Int (Maybe Collision.Ray)
    | UnholdCard
