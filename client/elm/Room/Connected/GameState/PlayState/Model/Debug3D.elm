module Debug3D exposing (debugView, entities)

import Game.Entity exposing (Entity3D)
import Game.Types as Game exposing (Context)
import Math.Matrix4 exposing (makeRotate, makeScale)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Quaternion
import Render.Primitives
import Render.Shaders
import Texture.State as Texture
import WebGL


entities : Float -> List (Entity3D {})
entities time =
    [ { position = vec3 0.2 0 0
      , rotation = Quaternion.zRotation <| (0.0001 * time)
      , scale = vec3 0.1 0.1 0.1
      }
    ]


debugView : Float -> Context -> List WebGL.Entity
debugView time ctx =
    let
        { camera3d, perspective, textures } =
            ctx
    in
    Texture.with textures "cardBack.png" <|
        \texture ->
            List.concat <|
                List.map
                    (\entity ->
                        [ Render.Primitives.quad Render.Shaders.matte <|
                            { rotation = Quaternion.makeRotate entity.rotation
                            , scale = makeScale entity.scale
                            , color = vec3 1 1 1
                            , pos = entity.position
                            , perspective = perspective
                            , camera = camera3d
                            , alpha = 1
                            }
                        , Render.Primitives.quad Render.Shaders.fragment <|
                            { rotation = Quaternion.makeRotate entity.rotation
                            , scale = makeScale entity.scale
                            , color = vec3 1 1 1
                            , pos = entity.position
                            , perspective = perspective
                            , camera = camera3d
                            , texture = texture
                            }
                        ]
                    )
                <|
                    entities time
