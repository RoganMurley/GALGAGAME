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
        Just (Slash _ d) ->
            case d of
                0 ->
                    Shaders.null

                otherwise ->
                    Shaders.slash

        Just (Heal _) ->
            Shaders.heal

        Just (Obliterate _) ->
            Shaders.obliterate

        Just (Draw _) ->
            Shaders.null

        Just (Bite _ _) ->
            Shaders.null

        Just (Reverse _) ->
            Shaders.null

        Just (Play _ _) ->
            Shaders.null

        Just (Transmute _ _ _) ->
            Shaders.null

        Just (GameEnd _) ->
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

        Bite w _ ->
            w

        Reverse w ->
            w

        Play w _ ->
            w

        Transmute w _ _ ->
            w

        GameEnd _ ->
            PlayerA

        Custom _ ->
            PlayerA


animShake : Maybe Anim -> Float -> Float
animShake anim tick =
    let
        baseMag =
            case anim of
                Just (Slash _ d) ->
                    5.0 * Ease.outQuad (toFloat d / 50.0)

                Just (Bite _ d) ->
                    5.0 * Ease.outQuad (toFloat d / 50.0)

                Just (Obliterate _) ->
                    20.0

                Just (Play _ _) ->
                    1.0

                otherwise ->
                    0.0

        mag =
            baseMag * (1.0 - Ease.outQuad (tick / animToResTickMax anim))
    in
        mag * 0.03 * (toFloat (((ceiling tick) * 1247823748932 + 142131) % 20) - 10)


animToResTickMax : Maybe Anim -> Float
animToResTickMax anim =
    case anim of
        Just (Reverse _) ->
            1500.0

        Just (Play _ _) ->
            500.0

        Just (Obliterate _) ->
            3000.0

        otherwise ->
            800.0
