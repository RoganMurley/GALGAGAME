module Render.Meshes exposing (quad, triangle)

import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Render.Types exposing (Vertex)
import WebGL exposing (Mesh)


type alias Attributes =
    { position : Vec3
    , coord : Vec2
    }


topRight : Attributes
topRight =
    { position = vec3 1 1 0
    , coord = vec2 0 1
    }


bottomRight : Attributes
bottomRight =
    { position = vec3 1 -1 0
    , coord = vec2 0 0
    }


bottomLeft : Attributes
bottomLeft =
    { position = vec3 -1 -1 0
    , coord = vec2 1 0
    }


topLeft : Attributes
topLeft =
    { position = vec3 -1 1 0
    , coord = vec2 1 1
    }


quad : Mesh Vertex
quad =
    WebGL.triangles
        [ ( topLeft, topRight, bottomLeft )
        , ( bottomLeft, topRight, bottomRight )
        ]


triangle : Mesh Vertex
triangle =
    WebGL.triangles
        [ ( topLeft, topRight, bottomLeft )
        ]
