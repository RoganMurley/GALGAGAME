module Holding.Types exposing (Holding(..))

import Card.Types exposing (Card)
import Hover exposing (HoverDamage)
import Math.Vector3 exposing (Vec3)
import Quaternion exposing (Quaternion)


type Holding
    = NoHolding
    | Holding
        { card : Card
        , handIndex : Int
        , dmg : ( HoverDamage, HoverDamage )
        , pos : Vec3
        , rot : Quaternion
        }
