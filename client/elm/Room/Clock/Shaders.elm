module Clock.Shaders exposing (..)

import Clock.Types exposing (Vertex)
import Clock.Uniforms exposing (Uniforms)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import WebGL exposing (Shader)


fragment : Shader {} (Uniforms {}) { vcoord : Vec2 }
fragment =
    [glsl|
        precision mediump float;

        uniform sampler2D texture;

        varying vec2 vcoord;

        void main ()
        {
            gl_FragColor = texture2D(texture, vcoord);
        }

    |]


matte : Shader {} { u | color : Vec3 } { vcoord : Vec2 }
matte =
    [glsl|
        precision mediump float;

        uniform vec3 color;

        varying vec2 vcoord;

        void main ()
        {
            gl_FragColor = vec4(color, 1.0);
        }

    |]


vertex : Shader Vertex (Uniforms u) { vcoord : Vec2 }
vertex =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec2 coord;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 scale;
        uniform mat4 rotation;
        uniform vec3 worldPos;
        uniform mat4 worldRot;

        varying vec2 vcoord;

        void main () {
            gl_Position = perspective * camera * worldRot * (vec4(worldPos, 1.0) + rotation * scale * vec4(position, 1.0));
            vcoord = coord.xy;
        }

    |]


noise : Shader {} (Uniforms { u | time : Float }) { vcoord : Vec2 }
noise =
    [glsl|
        precision mediump float;

        uniform vec2 resolution;
        uniform float time;

        varying vec2 vcoord;

        float random (in vec2 st) {
            return fract(sin(dot(st.xy,
                 vec2(12.9898,78.233)))
                     * 43758.5453123);
        }

        void main ()
        {
            vec2 st = gl_FragCoord.xy;
            float rnd = random(st * (1. + time));

            gl_FragColor = vec4(vec3(1.), rnd);
        }

    |]


disintegrate : Shader {} (Uniforms { u | time : Float, noise : WebGL.Texture }) { vcoord : Vec2 }
disintegrate =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform sampler2D texture;
        uniform sampler2D noise;

        varying vec2 vcoord;

        void main ()
        {
            vec4 color = texture2D(texture, vcoord);
            float random = texture2D(noise, vcoord).r;
            color.a *= floor((1. - time) + min(0.99, random));
            gl_FragColor = color;
        }

    |]


roundedBoxDisintegrate : Shader {} (Uniforms { u | time : Float }) { vcoord : Vec2 }
roundedBoxDisintegrate =
    [glsl|
        precision mediump float;

        uniform vec3 color;
        uniform sampler2D texture;
        uniform float time;

        varying vec2 vcoord;

        void main ()
        {
            vec2 pos = vec2(.5) - vcoord;

            float b = .4;
            float d = length(max(abs(pos) - b, .0));

            float a = smoothstep(d * 0.9, d * 1.1, .5 - b);

            float random = texture2D(texture, vcoord).r;
            a *= floor((1. - time) + min(0.99, random));

            gl_FragColor = vec4(color, a);
        }

    |]
