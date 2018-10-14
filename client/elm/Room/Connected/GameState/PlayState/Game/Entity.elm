module Game.Entity exposing (Entity)

import Math.Vector2 exposing (Vec2)


type alias Entity a =
    { a
        | position : Vec2
        , rotation : Float
        , scale : Float
    }
