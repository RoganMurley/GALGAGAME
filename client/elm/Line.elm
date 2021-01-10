module Line exposing (Line, view)

import Game.Types exposing (Context)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (Vec3, vec3)
import Render.Primitives
import Render.Shaders
import WebGL


type alias Line =
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    }


type alias Options =
    { thickness : Float
    , col : Vec3
    , alpha : Float
    }


view : Line -> Options -> Context -> List WebGL.Entity
view { x1, y1, x2, y2 } { alpha, col, thickness } { ortho, camera2d } =
    let
        w =
            abs (x2 - x1)

        h =
            abs (y2 - y1)

        x =
            min x1 x2 + w * 0.5

        y =
            min y1 y2 + h * 0.5

        len =
            0.5 * (sqrt <| w ^ 2 + h ^ 2)

        angle =
            atan2 w h
    in
    [ Render.Primitives.quad Render.Shaders.matte
        { rotation = makeRotate (0.5 * pi + angle) (vec3 0 0 1)
        , scale = makeScale3 len thickness 1
        , color = col
        , alpha = alpha
        , pos = vec3 x y 0
        , perspective = ortho
        , camera = camera2d
        }
    ]
