module Render.Uniforms exposing (Uniforms)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)


type alias Uniforms a =
    { a
        | rotation : Mat4
        , scale : Mat4
        , color : Vec3
        , worldPos : Vec3
        , worldRot : Mat4
        , perspective : Mat4
        , camera : Mat4
    }
