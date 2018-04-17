module Clock.Shaders exposing (..)

import Clock.Types exposing (Uniforms, Vertex)
import WebGL exposing (Shader)


fragment : Shader {} Uniforms {}
fragment =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2 resolution;
        uniform sampler2D texture;

        void main ()
        {
            vec2 uv = gl_FragCoord.xy / resolution.xy;

            gl_FragColor = texture2D(texture, uv);
        }

    |]


null : Shader {} a {}
null =
    [glsl|
        void main ()
        {
            discard;
        }
    |]


vertex : Shader Vertex a {}
vertex =
    [glsl|
        precision mediump float;

        attribute vec3 position;

        void main () {
            gl_Position = vec4(position, 1.);
        }

    |]
