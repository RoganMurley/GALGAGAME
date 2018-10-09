module Model.Entity exposing (GameEntity)

import Math.Vector2 exposing (Vec2)


type alias GameEntity a =
    { a
        | position : Vec2
        , rotation : Float
        , scale : Float
    }
