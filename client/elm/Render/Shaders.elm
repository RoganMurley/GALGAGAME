module Render.Shaders exposing (circleFragment, disintegrate, fragment, fragmentAlpha, fragmentTransmute, fullCircleFragment, laser, matte, ornate, spiral, starfield, trail, tunnel, vertex)

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
        uniform vec3 color;

        varying vec2 vcoord;

        void main ()
        {
            gl_FragColor = vec4(color, 1.) * texture2D(texture, vcoord);
        }

    |]


fragmentAlpha : Shader {} (Uniforms { texture : Texture, alpha : Float }) { vcoord : Vec2 }
fragmentAlpha =
    [glsl|
        precision mediump float;

        uniform sampler2D texture;
        uniform vec3 color;
        uniform float alpha;

        varying vec2 vcoord;

        void main ()
        {
            gl_FragColor = gl_FragColor = vec4(color, alpha) * texture2D(texture, vcoord);
        }

    |]


matte : Shader {} (Uniforms { alpha : Float }) { vcoord : Vec2 }
matte =
    [glsl|
        precision mediump float;

        uniform vec3 color;
        uniform float alpha;
        varying vec2 vcoord;

        void main ()
        {
            gl_FragColor = vec4(color, alpha);
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

        varying vec2 vcoord;

        void main () {
            gl_Position = perspective * camera * (vec4(pos, 1.0) + rotation * scale * vec4(position, 1.0));
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
        uniform vec3 color;

        varying vec2 vcoord;

        void main ()
        {
            vec4 final = vec4(color, 1.) * texture2D(texture, vcoord);
            float random = texture2D(noise, vcoord).r;
            final.a *= floor((1. - time) + min(0.99, random));
            gl_FragColor = final;
        }

    |]


circleFragment : Shader {} (Uniforms { mag : Float }) { vcoord : Vec2 }
circleFragment =
    [glsl|
        precision mediump float;

        uniform vec3 color;
        uniform float mag;

        varying vec2 vcoord;

        void main ()
        {
            float radius = .9;
            float dist = dot(2. * vcoord - 1., 2. * vcoord - 1.);
            float inner = smoothstep(radius * 1.05, radius * 1.03, dist);
            float outer = smoothstep(radius * 0.95, radius * 0.98, dist);
            float intensity = inner * outer;
            gl_FragColor = vec4(color, intensity * mag);
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


fragmentTransmute : Shader {} (Uniforms { time : Float, texture : Texture, finalTexture : Texture, finalColor : Vec3 }) { vcoord : Vec2 }
fragmentTransmute =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform sampler2D texture;
        uniform sampler2D finalTexture;
        uniform vec3 color;
        uniform vec3 finalColor;

        varying vec2 vcoord;

        void main ()
        {
            if (vcoord.x > time) {
                gl_FragColor = vec4(color, 1.) * texture2D(texture, vcoord);
            } else {
                gl_FragColor = vec4(finalColor, 1.) * texture2D(finalTexture, vcoord);
            }
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


trail : Shader {} (Uniforms { start : Vec2, end : Vec2 }) { vcoord : Vec2 }
trail =
    [glsl|
        precision mediump float;

        uniform vec3 color;
        uniform vec2 start;
        uniform vec2 end;

        varying vec2 vcoord;

        void main ()
        {
            float x = vcoord.x;
            float y = vcoord.y;

            float top = abs((end.y - start.y) * x - (end.x - start.x) * y + end.x * start.y - end.y * start.x);
            float length = sqrt(pow(end.y - start.y, 2.) + pow(end.x - start.x, 2.));
            float distanceToLine = top / length;

            float alpha = 0.;
            float maxDistance = .02;
            if (distanceToLine < maxDistance) {
                alpha = .4 - distanceToLine * 20.;
            }

            float upperY = max(end.y, start.y) + maxDistance;
            float lowerY = min(end.y, start.y) - maxDistance;
            float upperX = max(end.x, start.x) + maxDistance;
            float lowerX = min(end.x, start.x) - maxDistance;
            if (!(x >= lowerX && x <= upperX && y >= lowerY && y <= upperY)) {
                alpha = 0.;
            }

            float fade = sqrt(pow(end.y - y, 2.) + pow(end.x - x, 2.));
            alpha *=  1. - fade / length;

            gl_FragColor = vec4(color, alpha);
        }

    |]


starfield : Shader {} (Uniforms { brightness : Float, texture : Texture }) { vcoord : Vec2 }
starfield =
    [glsl|
        precision mediump float;

        uniform vec3 color;
        uniform float brightness;
        uniform sampler2D texture;

        varying vec2 vcoord;

        void main ()
        {
            vec2 uv = vcoord;
            vec3 random = texture2D(texture, uv).rgb;
            vec3 finalColor = color * (random + brightness);
            gl_FragColor = vec4(finalColor, 1.);
        }

    |]



-- With thanks to https://www.shadertoy.com/view/Mt2cRd


tunnel : Shader {} (Uniforms { depth : Float, spin : Float, texture : Texture }) { vcoord : Vec2 }
tunnel =
    [glsl|
        precision mediump float;

        uniform vec3 color;
        uniform float depth;
        uniform float spin;
        uniform sampler2D texture;

        varying vec2 vcoord;

        const float tau = 6.283;

        vec2 to_polar(vec2 xy){
            return vec2(length(xy), atan(xy.y, xy.x) / tau);
        }

        void main ()
        {
            vec2 uv = vcoord;
            vec2 cartesian = uv - vec2(.5);
            vec2 polar = to_polar(cartesian);

            vec2 add = vec2(-.0001 * depth, .132641 + spin / tau);
            vec2 mul = vec2(1., 12.);

            vec2 fpos = (vec2(-sqrt(1. / polar.x), polar.y) + add) * mul;

            //float dist = 1. - dot(2. * vcoord - 1., 2. * vcoord - 1.);
            float dist = 0.;
            gl_FragColor = texture2D(texture, fpos) -vec4(vec3(dist * dist), .0);
        }

    |]


spiral : Shader {} (Uniforms { progress : Float, texture : Texture }) { vcoord : Vec2 }
spiral =
    [glsl|
        precision mediump float;

        uniform vec3 color;
        uniform float progress;
        uniform sampler2D texture;

        varying vec2 vcoord;

        const float tau = 6.283;

        vec2 to_polar(vec2 xy){
            return vec2((1. - progress)*length(xy), atan(xy.y, xy.x) / tau);
        }

        void main ()
        {
            vec2 uv = vcoord;
            vec2 cartesian = uv - vec2(.5);
            vec2 polar = to_polar(cartesian);

            vec2 add = vec2(-.01 * progress, .132641 + .01 * progress);
            vec2 mul = vec2(.1, 1.);

            vec2 fpos = (vec2(-sqrt(1. / polar.x), polar.y) + add) * mul;

            vec4 sample = texture2D(texture, fpos);

            float dist = 0.;1. - dot(2. * vcoord - 1., 2. * vcoord - 1.);
            gl_FragColor = vec4(color, .95) * sample.a;
        }

    |]


laser : Shader {} (Uniforms { hurt : Float, progress : Float, texture : Texture }) { vcoord : Vec2 }
laser =
    [glsl|
        precision mediump float;

        uniform vec3 color;
        uniform float progress;
        uniform float hurt;
        uniform sampler2D texture;

        varying vec2 vcoord;

        const float tau = 6.283;

        vec2 to_polar(vec2 xy){
            return vec2(length(xy), atan(xy.y, xy.x) / tau);
        }

        void main ()
        {
            vec2 uv = vcoord;
            vec2 cartesian = uv - vec2(.5);
            vec2 polar = to_polar(cartesian);

            vec2 add = vec2(.0, .132641);
            vec2 mul = vec2(.05, 12.);

            vec2 fpos = (vec2(-sqrt(1. / polar.x), polar.y) + add) * mul;

            vec4 sample = texture2D(texture, fpos);
            float stepProgress =  progress - .2;
            gl_FragColor = vec4(color, hurt * step(sample.r, 1. - stepProgress) * sample.a);
        }

    |]
