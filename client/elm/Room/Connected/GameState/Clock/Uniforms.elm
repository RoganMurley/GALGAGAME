module Clock.Uniforms exposing (Uniforms, uniforms)

import Math.Matrix4 exposing (Mat4, identity, makeLookAt, makeOrtho, makeRotate)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Raymarch.Types exposing (Height, Width)
import WebGL exposing (Texture)


type alias Uniforms a =
    { a
        | resolution : Vec2
        , texture : Texture
        , rotation : Mat4
        , scale : Mat4
        , color : Vec3
        , worldPos : Vec3
        , worldRot : Mat4
        , perspective : Mat4
        , camera : Mat4
    }


uniforms : Float -> ( Width, Height ) -> Texture -> Vec3 -> Mat4 -> Mat4 -> Vec3 -> Uniforms {}
uniforms t ( width, height ) texture pos rot scale color =
    { resolution = vec2 (toFloat width) (toFloat height)
    , texture = texture
    , rotation = rot
    , scale = scale
    , color = color
    , worldPos = pos
    , worldRot = makeRotate 0 (vec3 0 0 1)
    , perspective = makeOrtho 0 (toFloat width / 2) (toFloat height / 2) 0 0.01 1000
    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
    }
