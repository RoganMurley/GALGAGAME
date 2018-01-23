module Animation.State exposing (..)

import Math.Vector2 exposing (vec2)
import Animation.Types exposing (Anim(..))
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
                1.0

            Just PlayerA ->
                1.0

            Just PlayerB ->
                0.0
    }


animToFragmentShader : Maybe Anim -> Shader {} Uniforms {}
animToFragmentShader anim =
    case anim of
        Just (Slash _) ->
            Shaders.slash

        Just (Heal _) ->
            Shaders.heal

        Just (Obliterate _) ->
            Shaders.obliterate

        Just (Draw _) ->
            Shaders.null

        Just (Custom s) ->
            unsafeShader s

        Nothing ->
            Shaders.null


getWhichPlayer : Anim -> WhichPlayer
getWhichPlayer anim =
    case anim of
        Slash w ->
            w

        Draw w ->
            w

        Obliterate w ->
            w

        Heal w ->
            w

        Custom _ ->
            PlayerA
