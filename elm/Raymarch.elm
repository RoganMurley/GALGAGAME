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
            5
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
                (uniforms time ( w, h ))
            ]


type alias Uniforms =
    { time : Float
    , resolution : Vec2
    }


uniforms : Float -> ( Width, Height ) -> Uniforms
uniforms theta ( width, height ) =
    { time = theta
    , resolution = vec2 (toFloat width) (toFloat height)
    }



-- Mesh


type alias Vertex =
    { position : Vec3
    }


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



-- Shaders


vertexShader : Shader Vertex Uniforms {}
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;

        uniform float time;

        void main () {
            gl_Position = vec4(position, 1.0);
        }

    |]


fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2 resolution;

        const float ITERATIONS = 16.0;
        const float VIEW_DISTANCE = 10.0;
        const float CLOUD_DENSITY = 0.5;
        const float OCTAVES = 5.0;

        const vec4 SKY_COLOR = vec4(0.02, 0.0178, 0.018, 1.0);
        const vec4 CLOUD_COLOR = vec4(0.9, 0.9, 0.9, 1.0);

        float hash(vec3 p)
        {
            p  = fract( p*0.3183099+.1 );
        	p *= 17.0;
            return fract( p.x*p.y*p.z*(p.x+p.y+p.z) );
        }

        float noise( in vec3 x )
        {
            vec3 p = floor(x);
            vec3 f = fract(x);
            f = f*f*(3.0-2.0*f);

            return mix(mix(mix( hash(p+vec3(0,0,0)),
                                hash(p+vec3(1,0,0)),f.x),
                           mix( hash(p+vec3(0,1,0)),
                                hash(p+vec3(1,1,0)),f.x),f.y),
                       mix(mix( hash(p+vec3(0,0,1)),
                                hash(p+vec3(1,0,1)),f.x),
                           mix( hash(p+vec3(0,1,1)),
                                hash(p+vec3(1,1,1)),f.x),f.y),f.z);
        }

        float fbm(vec3 pos)
        {
            float f = 0.0;
            for (float i = 0.0; i < OCTAVES; i++)
            {
                f += noise(pos) / pow(2.0, i + 1.0);
                pos *= 2.01;
            }
            f /= 1.0 - 1.0 / pow(2.0, OCTAVES + 1.0);
            return f;
        }

        void main ()
        {
          vec2 uv = gl_FragCoord.xy / resolution.xy;
          uv = uv * 2.0 - 1.0;
          uv.x *= resolution.x / resolution.y;

          vec3 ray = normalize(vec3(uv, 1.0));
          vec3 pos = vec3(0.0, 0.0, -time*0.2);

          float density = 0.0;

          // Raymarching
          for (float i = 0.0; i < ITERATIONS; i++)
          {
              float f = i / ITERATIONS;
              float alpha = smoothstep(0.0, ITERATIONS * 0.2, i) * (1.0 - f) * (1.0 - f);

              float denseClouds = smoothstep(CLOUD_DENSITY, 0.75, fbm(pos));
              density += denseClouds * alpha;

              pos += ray * f * VIEW_DISTANCE;
          }

          vec3 color = SKY_COLOR.rgb + (CLOUD_COLOR.rgb - 0.5) * (density / ITERATIONS) * 20.0 * CLOUD_COLOR.a;

          gl_FragColor = vec4(color, 1.0);
        }

    |]
