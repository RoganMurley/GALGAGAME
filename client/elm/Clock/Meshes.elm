module Clock.Meshes exposing (..)

import Math.Vector3 exposing (vec3)
import Clock.Types exposing (Vertex)
import WebGL exposing (Mesh, triangles)


quad : Mesh Vertex
quad =
    let
        topRight =
            { position = vec3 1 -1 0, coord = vec3 1 1 0 }

        bottomRight =
            { position = vec3 1 1 0, coord = vec3 1 0 0 }

        bottomLeft =
            { position = vec3 -1 1 0, coord = vec3 0 0 0 }

        topLeft =
            { position = vec3 -1 -1 0, coord = vec3 1 0 0 }
    in
        WebGL.triangles
            [ ( topLeft, topRight, bottomLeft )
            , ( bottomLeft, topRight, bottomRight )
            ]
