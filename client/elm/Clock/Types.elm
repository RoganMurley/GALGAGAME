module Clock.Types exposing (..)

import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import WebGL.Texture exposing (Texture)


type alias Model =
    { time : Float }


type alias Uniforms =
    { time : Float
    , resolution : Vec2
    , texture : Texture
    , rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    }


type alias Vertex =
    { position : Vec3
    , coord : Vec3
    }
