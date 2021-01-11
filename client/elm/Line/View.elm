module Line.View exposing (view)

import Game.Types exposing (Context)
import Line.Types exposing (Line, Options)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Shaders
import WebGL


view : Line -> Options -> Context -> List WebGL.Entity
view { x1, y1, x2, y2 } { alpha, col, thickness } { ortho, camera2d } =
    let
        w =
            x2 - x1

        h =
            y2 - y1

        x =
            min x1 x2 + abs w * 0.5

        y =
            min y1 y2 + abs h * 0.5

        len =
            sqrt <| w ^ 2 + h ^ 2

        angle =
            atan2 w h
    in
    [ Render.Primitives.quad Render.Shaders.matte
        { rotation = makeRotate (0.5 * pi - angle) (vec3 0 0 1)
        , scale = makeScale3 (0.5 * len) thickness 1
        , color = col
        , alpha = alpha
        , pos = vec3 x y 0
        , perspective = ortho
        , camera = camera2d
        }
    ]
