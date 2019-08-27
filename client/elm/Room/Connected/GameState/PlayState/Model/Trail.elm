module Trail exposing (view)

import Animation.State as Animation
import Animation.Types exposing (Anim(..))
import Colour
import Ease
import Game.Types exposing (Context, Hover(..))
import Hand.Entities exposing (handCardPosition)
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Shaders
import Util exposing (interp2D)
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Context -> List WebGL.Entity
view ({ anim, model, progress, radius, w, h, tick } as ctx) =
    case anim of
        Play PlayerA _ i ->
            let
                n : Int
                n =
                    List.length model.hand + 1

                base : Vec2
                base =
                    handCardPosition ctx PlayerA i n NoHover

                baseX : Float
                baseX =
                    Math.Vector2.getX base

                start : Vec2
                start =
                    interp2D (Ease.inBounce (tick / Animation.animMaxTick anim))
                        (vec2 (baseX / w) 0)
                        final

                end : Vec2
                end =
                    interp2D progress start final

                final : Vec2
                final =
                    vec2 0.5 (0.5 + (1 / h) * radius * 0.62)
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

                base : Vec2
                base =
                    handCardPosition ctx PlayerB i n NoHover

                baseX : Float
                baseX =
                    Math.Vector2.getX base

                start : Vec2
                start =
                    interp2D (Ease.inBounce (tick / Animation.animMaxTick anim))
                        (vec2 (baseX / w) 1)
                        final

                end : Vec2
                end =
                    interp2D progress start final

                final : Vec2
                final =
                    vec2 0.5 (0.5 + (1 / h) * radius * 0.62)
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
