module Clock.Wave exposing (..)

import Animation.State exposing (animToResTickMax)
import Animation.Types exposing (Anim(..))
import Clock.Primitives as Primitives
import Clock.Types exposing (ClockParams)
import Clock.Uniforms exposing (uniforms)
import Ease
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import Util exposing (floatInterp)
import WebGL
import WebGL.Texture exposing (Texture)


view : ClockParams -> Maybe ( Float, Maybe Anim ) -> Texture -> List WebGL.Entity
view { w, h, radius } resInfo texture =
    let
        locals =
            uniforms 0 ( floor w, floor h )

        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        anim =
            Maybe.join <|
                Maybe.map Tuple.second resInfo

        maxTick =
            animToResTickMax anim

        progress =
            Ease.outQuad <| resTick / maxTick

        sizeA =
            floatInterp progress 0 (3 * radius)

        sizeB =
            floatInterp progress (0.5 * radius) (3 * radius)

        sizeC =
            floatInterp progress (0.8 * radius) (3 * radius)
    in
        case anim of
            Just (Rotate _) ->
                [ Primitives.circle <|
                    locals texture
                        (vec3 (w / 2) (h / 2) 0)
                        (makeScale3 sizeA sizeA 1)
                        (makeRotate 0 (vec3 0 0 1))
                        (vec3 1 1 1)
                , Primitives.circle <|
                    locals texture
                        (vec3 (w / 2) (h / 2) 0)
                        (makeScale3 sizeB sizeB 1)
                        (makeRotate 0 (vec3 0 0 1))
                        (vec3 1 1 1)
                , Primitives.circle <|
                    locals texture
                        (vec3 (w / 2) (h / 2) 0)
                        (makeScale3 sizeC sizeC 1)
                        (makeRotate 0 (vec3 0 0 1))
                        (vec3 1 1 1)
                ]

            otherwise ->
                []
