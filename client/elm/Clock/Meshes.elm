module Clock.Meshes exposing (..)

import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (Vec3, add, scale, vec3)
import Clock.Types exposing (Vertex)
import WebGL exposing (Mesh, triangles)


quad : Float -> Mesh Vertex
quad s =
    let
        topRight =
            { position = scale s <| vec3 1 1 0, coord = vec2 1 1 }

        bottomRight =
            { position = scale s <| vec3 1 -1 0, coord = vec2 1 0 }

        bottomLeft =
            { position = scale s <| vec3 -1 -1 0, coord = vec2 0 0 }

        topLeft =
            { position = scale s <| vec3 -1 1 0, coord = vec2 0 1 }
    in
        WebGL.triangles
            [ ( topLeft, topRight, bottomLeft )
            , ( bottomLeft, topRight, bottomRight )
            ]
