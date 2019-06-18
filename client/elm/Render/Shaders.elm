module Render.Shaders exposing (circleFragment, disintegrate, fragment, fragmentAlpha, fragmentTransmute, fullCircleFragment, matte, ornate, roundedBoxDisintegrate, roundedBoxFragment, roundedBoxTransmute, vertex)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Render.Types exposing (Vertex)
import Render.Uniforms exposing (Uniforms)
import WebGL exposing (Shader)
import WebGL.Texture exposing (Texture)


fragment : Shader {} (Uniforms { texture : Texture }) { vcoord : Vec2 }
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


fragmentAlpha : Shader {} (Uniforms { texture : Texture, alpha : Float }) { vcoord : Vec2 }
fragmentAlpha =
    [glsl|
        precision mediump float;

        uniform sampler2D texture;
        uniform float alpha;

        varying vec2 vcoord;

        void main ()
        {
            vec4 color = texture2D(texture, vcoord);
            color.a *= alpha;
            gl_FragColor = color;
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
        uniform vec3 pos;
        uniform mat4 worldRot;

        varying vec2 vcoord;

        void main () {
            gl_Position = perspective * camera * worldRot * (vec4(pos, 1.0) + rotation * scale * vec4(position, 1.0));
            vcoord = coord.xy;
        }

    |]


disintegrate : Shader {} (Uniforms { u | time : Float, texture : Texture, noise : Texture }) { vcoord : Vec2 }
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


roundedBoxDisintegrate : Shader {} (Uniforms { time : Float, texture : Texture }) { vcoord : Vec2 }
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


circleFragment : Shader {} (Uniforms u) { vcoord : Vec2 }
circleFragment =
    [glsl|
        precision mediump float;

        uniform vec3 color;

        varying vec2 vcoord;

        void main ()
        {
            float radius = .9;
            float dist = dot(2. * vcoord - 1., 2. * vcoord - 1.);
            float inner = smoothstep(radius * 1.05, radius * 1.03, dist);
            float outer = smoothstep(radius * 0.95, radius * 0.98, dist);
            float intensity = inner * outer;
            gl_FragColor = vec4(color, intensity);
        }

    |]


fullCircleFragment : Shader {} (Uniforms { mag : Float }) { vcoord : Vec2 }
fullCircleFragment =
    [glsl|
        precision mediump float;

        uniform vec3 color;
        uniform float mag;

        varying vec2 vcoord;

        void main ()
        {
            float radius = .9;
            float dist = dot(2. * vcoord - 1., 2. * vcoord - 1.);
            float intensity = step(dist, radius);
            if (mag > 1. - vcoord.y) {
                gl_FragColor = vec4(color, intensity);
            } else {
                gl_FragColor = vec4(color, .0);
            }
        }

    |]


fragmentTransmute : Shader {} (Uniforms { time : Float, texture : Texture, finalTexture : Texture }) { vcoord : Vec2 }
fragmentTransmute =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform sampler2D texture;
        uniform sampler2D finalTexture;

        varying vec2 vcoord;

        void main ()
        {
            if (vcoord.x > time) {
                gl_FragColor = texture2D(texture, vcoord);
            } else {
                gl_FragColor = texture2D(finalTexture, vcoord);
            }
        }

    |]


roundedBoxTransmute : Shader {} (Uniforms { u | time : Float, finalColor : Vec3 }) { vcoord : Vec2 }
roundedBoxTransmute =
    [glsl|
        precision mediump float;

        uniform vec3 color;
        uniform vec3 finalColor;
        uniform float time;

        varying vec2 vcoord;

        void main ()
        {
            vec2 pos = vec2(.5) - vcoord;

            float b = .4;
            float d = length(max(abs(pos) - b, .0));

            float a = smoothstep(d * 0.9, d * 1.1, .5 - b);

            vec3 actualColor = vcoord.x > time ? color : finalColor;
            gl_FragColor = vec4(actualColor, a);
        }

    |]


roundedBoxFragment : Shader {} (Uniforms {}) { vcoord : Vec2 }
roundedBoxFragment =
    [glsl|
        precision mediump float;

        uniform vec3 color;

        varying vec2 vcoord;

        void main ()
        {
            vec2 pos = vec2(.5) - vcoord;

            float b = .4;
            float d = length(max(abs(pos) - b, .0));

            float a = smoothstep(d * 0.9, d * 1.1, .5 - b);
            gl_FragColor = vec4(color, a);
        }

    |]


ornate : Shader {} (Uniforms { amplitude : Float, frequency : Float, shift : Float, thickness : Float }) { vcoord : Vec2 }
ornate =
    [glsl|
        precision mediump float;

        uniform vec3 color;
        uniform float shift;
        uniform float frequency;
        uniform float amplitude;
        uniform float thickness;

        varying vec2 vcoord;

        void main ()
        {
            float x = amplitude * sin(frequency * 2. * 3.14 * vcoord.x + shift);
            float y =  2. * vcoord.y -1.;
            float realThickness = 10. * thickness * pow(1. - vcoord.x, 5.);
            gl_FragColor = vec4(color, abs(y - x) < realThickness);
        }

    |]
