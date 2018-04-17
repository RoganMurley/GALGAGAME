module Clock.Shaders exposing (..)

import Clock.Types exposing (Uniforms, Vertex)
import Math.Vector2 exposing (Vec2)
import WebGL exposing (Shader)


fragment : Shader {} Uniforms { vcoord : Vec2 }
fragment =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2 resolution;
        uniform sampler2D texture;

        varying vec2 vcoord;

        void main ()
        {
            vec2 uv = gl_FragCoord.xy / resolution.xy;

            gl_FragColor = texture2D(texture, vcoord);
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


vertex : Shader Vertex Uniforms { vcoord : Vec2 }
vertex =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 coord;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;

        varying vec2 vcoord;

        void main () {
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            vcoord = coord.xy;
        }

    |]
