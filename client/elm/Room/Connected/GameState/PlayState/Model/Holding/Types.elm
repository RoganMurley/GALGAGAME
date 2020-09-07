module Holding.Types exposing (Holding(..))

import Card.Types exposing (Card)
import Math.Vector3 exposing (Vec3)
import Model.Types exposing (Life)
import Quaternion exposing (Quaternion)


type Holding
    = NoHolding
    | Holding
        { card : Card
        , handIndex : Int
        , dmg : ( Life, Life )
        , pos : Vec3
        , rot : Quaternion
        }
