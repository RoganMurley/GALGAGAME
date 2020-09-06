module Holding.Types exposing (Holding(..))

import Card.Types exposing (Card)
import Math.Vector3 exposing (Vec3)


type Holding
    = NoHolding
    | Holding
        { card : Card
        , handIndex : Int
        , pos : Vec3
        }
