module Trail exposing (view)

import Animation.State as Animation
import Animation.Types exposing (Anim(..))
import Colour
import Ease
import Game.Types exposing (Context, Hover(..))
import Hand.Entities exposing (handCardPosition, playPosition)
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Shaders
import Util exposing (interp2D)
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


toShaderSpace : Float -> Float -> Vec2 -> Vec2
toShaderSpace w h screenSpace =
    let
        { x, y } =
            Math.Vector2.toRecord screenSpace
    in
    vec2 (x / w) (1 - y / h)


view : Context -> List WebGL.Entity
view ({ anim, model, progress, w, h, tick } as ctx) =
    let
        trailProgress : Float
        trailProgress =
            Ease.inBounce (tick / Animation.animMaxTick anim)
    in
    case anim of
        Play PlayerA _ i ->
            let
                n : Int
                n =
                    List.length model.hand + 1

                initial : Vec2
                initial =
                    toShaderSpace w h <|
                        handCardPosition ctx PlayerA i n NoHover

                final : Vec2
                final =
                    toShaderSpace w h <| playPosition ctx

                start : Vec2
                start =
                    interp2D trailProgress initial final

                end : Vec2
                end =
                    interp2D progress start final
            in
            [ Render.Primitives.quad Render.Shaders.trail
                { rotation = makeRotate pi (vec3 0 0 1)
                , scale = makeScale3 (0.5 * w) (0.5 * h) 1
                , color = Colour.card PlayerA
                , pos = vec3 (w * 0.5) (h * 0.5) 0
                , worldRot = makeRotate 0 (vec3 0 0 1)
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , start = start
                , end = end
                }
            ]

        Play PlayerB _ i ->
            let
                n : Int
                n =
                    model.otherHand + 1

                initial : Vec2
                initial =
                    toShaderSpace w h <|
                        handCardPosition ctx PlayerB i n NoHover

                final : Vec2
                final =
                    toShaderSpace w h <| playPosition ctx

                start : Vec2
                start =
                    interp2D trailProgress initial final

                end : Vec2
                end =
                    interp2D progress start final
            in
            [ Render.Primitives.quad Render.Shaders.trail
                { rotation = makeRotate pi (vec3 0 0 1)
                , scale = makeScale3 (0.5 * w) (0.5 * h) 1
                , color = Colour.card PlayerB
                , pos = vec3 (w * 0.5) (h * 0.5) 0
                , worldRot = makeRotate 0 (vec3 0 0 1)
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , start = start
                , end = end
                }
            ]

        _ ->
            []
