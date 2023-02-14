module Debug3D exposing (debugView, entities)

import Game.Entity exposing (Entity3D)
import Game.Types as Game exposing (Context)
import Math.Matrix4 exposing (makeRotate, makeScale)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Quaternion
import Render.Primitives
import Render.Shaders
import WebGL


entities : List (Entity3D {})
entities =
    [ { position = vec3 0 0 0
      , rotation = Quaternion.identity
      , scale = vec3 0.1 1 1
      }
    ]


debugView : Context -> List WebGL.Entity
debugView ctx =
    let
        { camera3d, perspective } =
            ctx
    in
    List.map
        (\entity ->
            Render.Primitives.triangle Render.Shaders.matte <|
                { rotation = Quaternion.makeRotate entity.rotation
                , scale = makeScale entity.scale
                , color = vec3 1 1 1
                , pos = entity.position
                , perspective = perspective
                , camera = camera3d
                , alpha = 1
                }
        )
        entities
