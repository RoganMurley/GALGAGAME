module Raymarch.State exposing (..)

import Math.Vector2 exposing (vec2)
import Raymarch.Types exposing (Height, Uniforms, Width)


uniforms : Float -> ( Width, Height ) -> Uniforms
uniforms theta ( width, height ) =
    { time = theta
    , resolution = vec2 (toFloat width) (toFloat height)
    }
