module DebugView exposing (debugView)

import Game.Types as Game exposing (Context)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Quaternion
import Render.Primitives
import Render.Shaders
import WebGL


debugView : Context -> List WebGL.Entity
debugView ctx =
    let
        { camera3d, perspective } =
            ctx
    in
    [ Render.Primitives.triangle Render.Shaders.matte <|
        { rotation = Quaternion.makeRotate Quaternion.identity
        , scale = makeScale3 0.1 0.1 0.1
        , color = vec3 1 1 1
        , pos = vec3 0 0 0
        , perspective = perspective
        , camera = camera3d
        , alpha = 1
        }
    ]
