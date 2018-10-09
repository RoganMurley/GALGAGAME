module Render.Types exposing (Params, Vertex)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)


type alias Params =
    { time : Float
    , w : Int
    , h : Int
    }


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }
