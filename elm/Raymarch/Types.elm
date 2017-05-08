module Raymarch.Types exposing (..)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)


type alias Time =
    Float


type alias Width =
    Int


type alias Height =
    Int


type Params
    = Params Time ( Width, Height )


type alias Uniforms =
    { time : Float
    , resolution : Vec2
    }


type alias Vertex =
    { position : Vec3
    }
