module Holding.Types exposing (Holding(..))

import Card.Types exposing (Card)
import Math.Vector3 exposing (Vec3)


type Holding
    = NoHolding
    | Holding Card Int Vec3
