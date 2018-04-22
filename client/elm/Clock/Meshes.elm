module Clock.Meshes exposing (..)

import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (Vec3, add, scale, vec3)
import Clock.Types exposing (Vertex)
import WebGL exposing (Mesh, triangles)


quad : Vec3 -> Float -> Mesh Vertex
quad offset s =
    let
        modifier : Vec3 -> Vec3
        modifier =
            add offset << scale s

        topRight =
            { position = modifier <| vec3 1 1 0, coord = vec2 1 1 }

        bottomRight =
            { position = modifier <| vec3 1 -1 0, coord = vec2 1 0 }

        bottomLeft =
            { position = modifier <| vec3 -1 -1 0, coord = vec2 0 0 }

        topLeft =
            { position = modifier <| vec3 -1 1 0, coord = vec2 0 1 }
    in
        WebGL.triangles
            [ ( topLeft, topRight, bottomLeft )
            , ( bottomLeft, topRight, bottomRight )
            ]
