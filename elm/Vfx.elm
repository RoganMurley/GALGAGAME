module Vfx exposing (..)

{-
   Rotating cube with colored sides.
-}

import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import WebGL exposing (Mesh, Shader)
import Messages exposing (Msg)


type alias Time =
    Float


type alias Width =
    Int


type alias Height =
    Int


type Params
    = Params Time ( Width, Height )


view : Params -> Float -> Html Msg
view (Params theta ( w, h )) intensity =
    WebGL.toHtml
        [ width w
        , height h
        , style [ ( "position", "absolute" ), ( "top", "0" ), ( "z-index", "-1" ) ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            quadMesh
            (uniforms theta intensity)
        ]


type alias Uniforms =
    { perspective : Mat4
    , time : Float
    , intensity : Float
    }


uniforms : Float -> Float -> Uniforms
uniforms theta intensity =
    { perspective = Mat4.makeOrtho2D 0 1 0 1
    , time = theta
    , intensity = intensity
    }



-- Mesh


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


quadMesh : Mesh Vertex
quadMesh =
    let
        v0 =
            vec3 1 0 0

        v1 =
            vec3 1 1 0

        v2 =
            vec3 0 1 0

        v3 =
            vec3 0 0 0
    in
        WebGL.triangles (face Color.red v0 v1 v2 v3)


face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face rawColor a b c d =
    let
        color =
            let
                c =
                    Color.toRgb rawColor
            in
                vec3
                    (toFloat c.red / 255)
                    (toFloat c.green / 255)
                    (toFloat c.blue / 255)

        vertex position =
            Vertex color position
    in
        [ ( vertex a, vertex b, vertex c )
        , ( vertex c, vertex d, vertex a )
        ]



-- Shaders


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        uniform float time;
        uniform float intensity;

        void main () {
            float redness = clamp(0.0, 1.0, intensity * (sin(gl_FragCoord.y*0.001 - 0.4) - abs(cos(gl_FragCoord.x*0.003)*0.1)));
            gl_FragColor = vec4(redness, 0.0, 0.0, 0.0);
        }

    |]
