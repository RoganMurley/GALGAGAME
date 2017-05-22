module Raymarch.Meshes exposing (quadMesh)

import Math.Vector3 exposing (vec3)
import Raymarch.Types exposing (Vertex)
import WebGL exposing (Mesh, triangles)


quadMesh : Mesh Vertex
quadMesh =
    let
        topRight =
            { position = vec3 1 -1 0 }

        bottomRight =
            { position = vec3 1 1 0 }

        bottomLeft =
            { position = vec3 -1 1 0 }

        topLeft =
            { position = vec3 -1 -1 0 }
    in
        WebGL.triangles
            [ ( topLeft, topRight, bottomLeft )
            , ( bottomLeft, topRight, bottomRight )
            ]
