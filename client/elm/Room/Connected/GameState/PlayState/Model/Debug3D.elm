module Debug3D exposing (debugView, entities)

import Game.Entity exposing (Entity3D)
import Game.Types exposing (Context)
import Math.Matrix4 exposing (makeScale)
import Math.Vector3 as Vector3 exposing (vec3)
import Quaternion
import Render.Primitives
import Render.Shaders
import Texture.State as Texture
import WebGL


entities : Float -> List (Entity3D {})
entities time =
    [ { position = vec3 0.7 0 0.1
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
                            , scale = makeScale <| Vector3.sub (vec3 0.03 0 0) entity.scale
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
