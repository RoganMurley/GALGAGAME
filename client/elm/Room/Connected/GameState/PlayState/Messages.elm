module PlayState.Messages exposing (Msg(..), PlayingOnly(..), TurnOnly(..))

import Card.Types exposing (Card)
import Collision
import Hover exposing (HoverDamage, HoverOther, HoverSelf)
import Leaderboard.Types exposing (Leaderboard)
import Math.Vector3 exposing (Vec3)
import Stats exposing (StatChange)


type Msg
    = HoverOtherOutcome HoverOther
    | DamageOutcome ( HoverDamage, HoverDamage )
    | PlayingOnly PlayingOnly
    | ReplaySaved String
    | GotoReplay String
    | GotoComputerGame
    | StatChange StatChange
    | SetLeaderboard Leaderboard
    | ServerTimeLeft Float
    | SkipAftermath
    | NoOp


type PlayingOnly
    = Rematch
    | TurnOnly TurnOnly
    | HoverCard HoverSelf
    | IllegalPass
    | TickleWheel Int


type TurnOnly
    = EndTurn
    | PlayCard Card Int Vec3
    | HoldCard Card Int (Maybe Collision.Ray)
    | UnholdCard
