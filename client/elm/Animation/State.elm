module Animation.State exposing (..)

import Animation.Shaders as Shaders
import Animation.Types exposing (Anim(..), FragShader(..), Uniforms)
import Ease
import Math.Vector2 exposing (vec2)
import Raymarch.Types exposing (Height, Width)
import Texture.State as Texture
import Texture.Types as Texture
import WebGL exposing (Shader, unsafeShader)
import WebGL.Texture exposing (Texture)
import WhichPlayer.Types exposing (WhichPlayer(..))


uniforms : Float -> Maybe WhichPlayer -> ( Width, Height ) -> Uniforms {}
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


animToFragShader : Anim -> FragShader
animToFragShader anim =
    case anim of
        Slash _ d ->
            case d of
                0 ->
                    BaseShader Shaders.null

                otherwise ->
                    BaseShader Shaders.slash

        Heal _ ->
            BaseShader Shaders.heal

        Obliterate _ ->
            BaseShader Shaders.obliterate

        Overdraw _ _ ->
            TexturedShader Shaders.overdraw

        Adhoc _ name _ ->
            case name of
                otherwise ->
                    BaseShader Shaders.null

        Custom s ->
            BaseShader <| unsafeShader s

        otherwise ->
            BaseShader Shaders.null


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

        Adhoc w _ _ ->
            w

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
            1000.0

        Just (Overdraw _ _) ->
            1000.0

        Just (Obliterate _) ->
            3000.0

        Just (GameEnd _) ->
            2500.0

        Just (Rotate _) ->
            1000.0

        otherwise ->
            800.0


animToTexture : Anim -> Texture.Model -> Maybe Texture
animToTexture anim textures =
    case anim of
        Overdraw _ _ ->
            Texture.load textures "cross"

        otherwise ->
            Nothing
