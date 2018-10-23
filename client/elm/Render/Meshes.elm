module Render.Meshes exposing (quad)

import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Render.Types exposing (Vertex)
import WebGL exposing (Mesh)


quad : Mesh Vertex
quad =
    let
        topRight =
            { position = vec3 1 1 0, coord = vec2 0 1 }

        bottomRight =
            { position = vec3 1 -1 0, coord = vec2 0 0 }

        bottomLeft =
            { position = vec3 -1 -1 0, coord = vec2 1 0 }

        topLeft =
            { position = vec3 -1 1 0, coord = vec2 1 1 }
    in
    WebGL.triangles
        [ ( topLeft, topRight, bottomLeft )
        , ( bottomLeft, topRight, bottomRight )
        ]
