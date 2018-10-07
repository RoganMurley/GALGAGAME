module Animation.State exposing (..)

import Animation.Types exposing (Anim(..), FragShader(..), Uniforms)
import Ease
import Texture.State as Texture
import Texture.Types as Texture
import WebGL.Texture exposing (Texture)
import WhichPlayer.Types exposing (WhichPlayer(..))


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

        Reflect w ->
            w

        Reverse w ->
            w

        Play w _ _ ->
            w

        Transmute w _ _ ->
            w

        Overdraw w _ ->
            w

        GameStart w ->
            w

        GameEnd _ ->
            PlayerA

        Rotate w ->
            w

        Windup w ->
            w


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

                Just (Play _ _ _) ->
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
        Just (Draw _) ->
            500.0

        Just (Reverse _) ->
            1500.0

        Just (Play _ _ _) ->
            500.0

        Just (Overdraw _ _) ->
            1000.0

        Just (Obliterate _) ->
            1000.0

        Just (GameEnd _) ->
            2500.0

        Just (Rotate _) ->
            1000.0

        Just (Windup _) ->
            300.0

        otherwise ->
            800.0


animToTexture : Anim -> Texture.Model -> Maybe Texture
animToTexture anim textures =
    case anim of
        Overdraw _ _ ->
            Texture.load textures "cross"

        otherwise ->
            Nothing
