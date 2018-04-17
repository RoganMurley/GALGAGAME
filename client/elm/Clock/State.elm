module Clock.State exposing (..)

import Clock.Types exposing (Model, Uniforms)
import Math.Vector2 exposing (vec2)
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
uniforms theta ( width, height ) texture =
    { time = theta
    , resolution = vec2 (toFloat width) (toFloat height)
    , texture = texture
    }
