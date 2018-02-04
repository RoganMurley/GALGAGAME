module Animation.State exposing (..)

import Animation.Shaders as Shaders
import Animation.Types exposing (Anim(..), Uniforms)
import Ease
import Math.Vector2 exposing (vec2)
import Raymarch.Types exposing (Height, Width)
import WebGL exposing (Shader, unsafeShader)
import WhichPlayer.Types exposing (WhichPlayer(..))


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
        Just (Slash _ _) ->
            Shaders.slash

        Just (Heal _) ->
            Shaders.heal

        Just (Obliterate _) ->
            Shaders.obliterate

        Just (Draw _) ->
            Shaders.null

        Just (Reverse _) ->
            Shaders.null

        Just (Custom s) ->
            unsafeShader s

        Nothing ->
            Shaders.null


getWhichPlayer : Anim -> WhichPlayer
getWhichPlayer anim =
    case anim of
        Slash w _ ->
            w

        Draw w ->
            w

        Obliterate w ->
            w

        Heal w ->
            w

        Reverse w ->
            w

        Custom _ ->
            PlayerA


animToShake : Anim -> Float
animToShake anim =
    case anim of
        Slash _ d ->
            5.0 * Ease.outQuad (toFloat d / 50.0)

        Heal _ ->
            0.0

        Obliterate _ ->
            0.0

        Draw _ ->
            0.0

        otherwise ->
            0.0
