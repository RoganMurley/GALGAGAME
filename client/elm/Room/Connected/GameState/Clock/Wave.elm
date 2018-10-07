module Clock.Wave exposing (..)

import Animation.State as Animation
import Animation.Types exposing (Anim(..))
import Clock.Primitives as Primitives
import Clock.Types exposing (ClockParams)
import Clock.Uniforms exposing (uniforms)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Maybe.Extra as Maybe
import Util exposing (interpFloat)
import WebGL
import WebGL.Texture exposing (Texture)


view : ClockParams -> Maybe ( Float, Maybe Anim ) -> Texture -> List WebGL.Entity
view { w, h, radius } resInfo texture =
    let
        locals =
            uniforms ( floor w, floor h )

        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        anim =
            Maybe.join <|
                Maybe.map Tuple.second resInfo

        progress =
            case anim of
                Just (Heal _) ->
                    1 - Animation.progress anim resTick

                _ ->
                    Animation.progress anim resTick

        waveSize =
            interpFloat progress 0 (3 * radius)
    in
        case anim of
            Just (Slash _ _) ->
                [ Primitives.circle <|
                    locals texture
                        (vec3 (w / 2) (h / 2) 0)
                        (makeScale3 waveSize waveSize 1)
                        (makeRotate 0 (vec3 0 0 1))
                        (vec3 1 0 0)
                ]

            Just (Bite _ _) ->
                [ Primitives.circle <|
                    locals texture
                        (vec3 (w / 2) (h / 2) 0)
                        (makeScale3 waveSize waveSize 1)
                        (makeRotate 0 (vec3 0 0 1))
                        (vec3 1 0 0)
                ]

            Just (Heal _) ->
                [ Primitives.circle <|
                    locals texture
                        (vec3 (w / 2) (h / 2) 0)
                        (makeScale3 waveSize waveSize 1)
                        (makeRotate 0 (vec3 0 0 1))
                        (vec3 0 1 0)
                ]

            _ ->
                []
