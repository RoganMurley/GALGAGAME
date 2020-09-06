module Holding.Types exposing (Holding(..))

import Card.Types exposing (Card)
import Math.Vector3 exposing (Vec3)
import Quaternion exposing (Quaternion)


type Holding
    = NoHolding
    | Holding
        { card : Card
        , handIndex : Int
        , pos : Vec3
        , rot : Quaternion
        }
