module Ripple.Types exposing (Ripple)

import Math.Vector2 exposing (Vec2)


type alias Ripple =
    { progress : Float
    , pos : Vec2
    }
