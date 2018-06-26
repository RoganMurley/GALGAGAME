module Clock.Stack exposing (..)

import Animation.Types exposing (Anim(..))
import Clock.Primitives as Primitives
import Clock.Shaders
import Clock.State exposing (animToResTickMax, clockFace, uniforms)
import Clock.Types exposing (ClockParams)
import Ease
import Math.Matrix4 exposing (Mat4, makeRotate, makeScale3)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import Stack.Types exposing (Stack, StackCard)
import WebGL
import WebGL.Texture exposing (Texture)
import WhichPlayer.Types exposing (WhichPlayer(..))


view : ClockParams -> Stack -> Maybe ( Float, Maybe Anim ) -> Texture -> List WebGL.Entity
view { w, h, radius } finalStack resInfo texture =
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

                Just (Play _ _ _) ->
                    1 - (Ease.inQuad <| resTick / maxTick)

                otherwise ->
                    0

        makeCard : ( WhichPlayer, Vec3, Mat4 ) -> List WebGL.Entity
        makeCard ( which, pos, rot ) =
            [ Primitives.roundedBox <|
                uniforms 0
                    ( floor w, floor h )
                    texture
                    pos
                    rot
                    (makeScale3 (0.7 * 0.13 * radius) (0.13 * radius) 1)
                    (case which of
                        PlayerA ->
                            vec3 0.18 0.49 0.62

                        PlayerB ->
                            vec3 0.52 0.1 0.2
                    )
            , Primitives.quad Clock.Shaders.fragment <|
                uniforms 0
                    ( floor w, floor h )
                    texture
                    pos
                    rot
                    (makeScale3 (0.13 * radius) (0.13 * radius) 1)
                    (vec3 1 1 1)
            ]

        stack : Stack
        stack =
            case anim of
                Just (Rotate _) ->
                    List.drop 1 finalStack

                otherwise ->
                    finalStack

        points =
            clockFace
                stack
                (vec3 (w / 2) (h / 2) 0)
                (0.615 * radius)
                progress
    in
        case anim of
            otherwise ->
                List.concat <| List.map makeCard points
