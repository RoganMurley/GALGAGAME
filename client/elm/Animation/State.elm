module Animation.State exposing (..)

import Math.Vector2 exposing (vec2)
import Card.Types exposing (Anim(..))
import Model.Types exposing (WhichPlayer(..))
import Animation.Shaders as Shaders
import Animation.Types exposing (Uniforms)
import Raymarch.Types exposing (Height, Width)
import WebGL exposing (Shader, unsafeShader)


uniforms : Float -> Maybe WhichPlayer -> ( Width, Height ) -> Uniforms
uniforms theta which ( width, height ) =
    { time = theta
    , resolution = vec2 (toFloat width) (toFloat height)
    , flipper =
        case which of
            Nothing ->
                0.0

            Just PlayerA ->
                0.0

            Just PlayerB ->
                1.0
    }


animToFragmentShader : Maybe Anim -> Shader {} Uniforms {}
animToFragmentShader params =
    case params of
        Just Slash ->
            Shaders.slash

        Just Obliterate ->
            Shaders.obliterate

        Just Heal ->
            Shaders.heal

        Just (Custom s) ->
            unsafeShader s

        Nothing ->
            Shaders.null
