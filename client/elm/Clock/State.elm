module Clock.State exposing (..)

import Clock.Types exposing (Model, Uniforms)
import Math.Matrix4 exposing (makeLookAt, makePerspective, makeRotate, mul)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Raymarch.Types exposing (Height, Width)
import WebGL exposing (Texture)


init : Model
init =
    { time = 0.0
    }


tick : Model -> Float -> Model
tick ({ time } as model) dt =
    { model | time = time + dt }


uniforms : Float -> ( Width, Height ) -> Texture -> Uniforms
uniforms t ( width, height ) texture =
    { time = t
    , resolution = vec2 (toFloat width) (toFloat height)
    , texture = texture
    , rotation = mul (makeRotate (3 * t) (vec3 0 1 0)) (makeRotate (2 * t) (vec3 1 0 0))
    , perspective = makePerspective 45 1 0.01 100
    , camera = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    }
