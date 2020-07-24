module Game.Entity exposing (Entity, Entity3D)

import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Quaternion exposing (Quaternion)


type alias Entity a =
    { a
        | position : Vec2
        , rotation : Float
        , scale : Float
    }


type alias Entity3D a =
    { a
        | position : Vec3
        , rotation : Quaternion
        , scale : Float
    }
