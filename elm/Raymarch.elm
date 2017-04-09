module Raymarch exposing (..)

{-
   Rotating cube with colored sides.
-}

import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
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


view : Params -> Html Msg
view (Params theta ( w, h )) =
    let
        time =
            theta / 1000

        downscale =
            3
    in
        WebGL.toHtml
            [ width (w // downscale)
            , height (h // downscale)
            , style [ ( "position", "absolute" ), ( "top", "0" ), ( "z-index", "-999" ), ( "width", "100%" ), ( "height", "100%" ) ]
            ]
            [ WebGL.entityWith []
                vertexShader
                fragmentShader
                quadMesh
                (uniforms time (Mat4.makeRotate 0 (Vec3.vec3 0 0 1.0)) (Vec3.vec3 1.0 1.0 0))
            , WebGL.entityWith []
                vertexShader
                fragmentShader
                quadMesh
                (uniforms time (Mat4.makeRotate pi (Vec3.vec3 0 0 1.0)) (Vec3.vec3 1.0 1.0 0))
            ]


type alias Uniforms =
    { perspective : Mat4
    , time : Float
    , rotation : Mat4
    , translation : Vec3
    }


uniforms : Float -> Mat4.Mat4 -> Vec3 -> Uniforms
uniforms theta rotation translation =
    { perspective = Mat4.makeOrtho2D 0 1 0 1
    , time = theta
    , rotation = rotation
    , translation = translation
    }



-- Mesh


type alias Vertex =
    { position : Vec3
    , uv : Vec2
    }


quadMesh : Mesh Vertex
quadMesh =
    let
        topRight =
            { position = vec3 0.5 -0.5 0, uv = vec2 1 0 }

        bottomRight =
            { position = vec3 0.5 0.5 0, uv = vec2 1 1 }

        bottomLeft =
            { position = vec3 -0.5 0.5 0, uv = vec2 0 1 }

        topLeft =
            { position = vec3 -0.5 -0.5 0, uv = vec2 0 0 }
    in
        WebGL.triangles
            [ ( topLeft, topRight, bottomLeft )
            , ( bottomLeft, topRight, bottomRight )
            ]



-- Shaders


vertexShader : Shader Vertex Uniforms { vUv : Vec2 }
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec2 uv;

        uniform mat4 perspective;
        uniform mat4 rotation;
        uniform vec3 translation;
        uniform float time;

        varying vec2 vUv;

        void main () {
            vUv = uv;
            gl_Position = perspective * rotation * vec4(position, 1.0) + vec4(translation, 0);
        }

    |]


fragmentShader : Shader {} Uniforms { vUv : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform float time;
        varying vec2 vUv;

        const int ITERATIONS = 64;
        const float Z_MAX = 40.0;
        const float EPSILON = 0.01;
        const vec3 CAMERA_POS = vec3(0.0, 0.0, -5.0);

        const float RADIUS = 1.0;

        float map(vec3 pos)
        {
            return length(pos) - RADIUS;
        }

        vec3 surface(vec3 pos)
        {
            const vec2 eps = vec2(0.0, EPSILON);

            float ambientIntensity = 0.1;
            vec3 lightDir = vec3(0.0, -0.5, 0.5);

            vec3 normal = normalize(vec3(
                map(pos + eps.yxx) - map(pos - eps.yxx),
                map(pos + eps.xyx) - map(pos - eps.xyx),
                map(pos + eps.xxy) - map(pos - eps.xxy))
            );

            float diffuse = ambientIntensity + max(dot(-lightDir, normal), 0.0);

            return vec3(diffuse, diffuse, diffuse);
        }

        void main ()
        {
            vec2 uv = vUv - 0.5;
            vec3 camUp = vec3(0.0, 1.0, 0.0);
            vec3 camForward = vec3(0.0, 0.0, 1.0);
            vec3 camRight = vec3(1.0, 0.0, 0.0);

            vec3 ray = camUp * uv.y + camRight * uv.x + camForward;
            vec3 color = vec3(0.0, 0.0, 0.0);
            vec3 pos = CAMERA_POS;

            // Raymarching
            for (int i = 0; i < ITERATIONS; ++i)
            {
                float distance = map(pos);

                if (distance < EPSILON)
                {
                    color = surface(pos);
                    break;
                }

                pos += ray * distance;

                if (distance > Z_MAX)
                {
                    break;
                }
            }

            gl_FragColor = vec4(color, 1.0);
        }

    |]
