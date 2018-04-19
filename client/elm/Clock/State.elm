module Clock.State exposing (..)

import Clock.Types exposing (Model, Uniforms)
import Math.Matrix4 exposing (Mat4, identity, makeLookAt, makePerspective, makeRotate, mul)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (Vec3, vec3)
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
    , rotation = makeRotate -t (vec3 0 0 1)
    , perspective = makePerspective 45 ((toFloat width) / (toFloat height)) 0.01 100
    , camera = makeLookAt (vec3 0 0 (4 + (sin t))) (vec3 0 0 0) (vec3 0 1 0)
    }


cameraUniforms : Vec3 -> { camera : Mat4, perspective : Mat4, rotation : Mat4, color : Vec3 }
cameraUniforms color =
    let
        t =
            0
    in
        { rotation = mul (makeRotate (3 * t) (vec3 0 1 0)) (makeRotate (2 * t) (vec3 1 0 0))
        , perspective = makePerspective 45 1 0.01 100
        , camera = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
        , color = color
        }


clockFace : Int -> Vec3 -> Float -> List Vec3
clockFace n origin radius =
    let
        indexes : List Int
        indexes =
            List.range 0 (n - 1)

        genPoint : Int -> Vec3
        genPoint i =
            Math.Vector3.add origin (offset i)

        offset : Int -> Vec3
        offset i =
            let
                rot =
                    (toFloat i) * 2.0 * pi / (toFloat n)
            in
                Math.Vector3.scale radius <|
                    vec3 (sin rot) (cos rot) 0
    in
        List.map genPoint indexes
