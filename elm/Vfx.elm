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


view : Params -> Float -> Float -> Html Msg
view (Params theta ( w, h )) lowerIntensity upperIntensity =
    WebGL.toHtml
        [ width w
        , height h
        , style [ ( "position", "absolute" ), ( "top", "0" ), ( "z-index", "-999" ) ]
        ]
        [ WebGL.entityWith []
            vertexShader
            fragmentShader
            (quadMesh
                (if upperIntensity > 0 then
                    Color.red
                 else
                    Color.green
                )
            )
            (uniforms theta (abs upperIntensity) (Mat4.makeRotate 0 (Vec3.vec3 0 0 1.0)) (Vec3.vec3 1.0 1.0 0))
        , WebGL.entityWith []
            vertexShader
            fragmentShader
            (quadMesh
                (if lowerIntensity > 0 then
                    Color.red
                 else
                    Color.green
                )
            )
            (uniforms theta (abs lowerIntensity) (Mat4.makeRotate pi (Vec3.vec3 0 0 1.0)) (Vec3.vec3 1.0 1.0 0))
        ]


type alias Uniforms =
    { perspective : Mat4
    , time : Float
    , intensity : Float
    , rotation : Mat4
    , translation : Vec3
    }


uniforms : Float -> Float -> Mat4.Mat4 -> Vec3 -> Uniforms
uniforms theta intensity rotation translation =
    { perspective = Mat4.makeOrtho2D 0 1 0 1
    , time = theta
    , intensity = intensity
    , rotation = rotation
    , translation = translation
    }



-- Mesh


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


quadMesh : Color -> Mesh Vertex
quadMesh color =
    let
        v0 =
            vec3 0.5 -0.5 0

        v1 =
            vec3 0.5 0.5 0

        v2 =
            vec3 -0.5 0.5 0

        v3 =
            vec3 -0.5 -0.5 0
    in
        WebGL.triangles (face color v0 v1 v2 v3)


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


vertexShader : Shader Vertex Uniforms { vcolor : Vec3, posy : Float }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 rotation;
        uniform vec3 translation;
        varying vec3 vcolor;
        varying float posy;

        void main () {
            gl_Position = perspective * rotation * vec4(position, 1.0) + vec4(translation, 0);
            vcolor = color;
            posy = position.y;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3, posy : Float }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform float intensity;
        uniform float time;
        varying vec3 vcolor;
        varying float posy;

        void main () {
            gl_FragColor = vec4(vcolor, 0.5) * posy * intensity;
            if (gl_FragColor.w < 0.0001) {
              discard;
            }
        }

    |]
