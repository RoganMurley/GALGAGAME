module Clock.Stack exposing (..)

import Animation.Types exposing (Anim(..))
import Animation.State exposing (animToResTickMax)
import Clock.Primitives as Primitives
import Clock.Shaders
import Clock.State exposing (uniforms)
import Clock.Types exposing (ClockParams)
import Ease
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import Stack.Types exposing (Stack)
import WebGL
import WebGL.Texture exposing (Texture)


view : ClockParams -> Stack -> Maybe ( Float, Maybe Anim ) -> Texture -> List WebGL.Entity
view { w, h, radius } stack resInfo texture =
    let
        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        anim =
            Maybe.join <|
                Maybe.map Tuple.second resInfo

        maxTick =
            animToResTickMax anim

        progress =
            case anim of
                Just (Rotate _) ->
                    Ease.inQuad <| resTick / maxTick

                otherwise ->
                    0

        makeCard ( pos, rot ) =
            Primitives.quad Clock.Shaders.fragment <|
                uniforms 0
                    ( floor w, floor h )
                    texture
                    pos
                    rot
                    (makeScale3 (0.13 * radius) (0.13 * radius) 1)
                    (vec3 1 1 1)

        points =
            Clock.State.clockFace
                (List.length stack)
                (vec3 (w / 2) (h / 2) 0)
                (0.62 * radius)
                progress
    in
        case anim of
            Nothing ->
                []

            otherwise ->
                List.map makeCard points
