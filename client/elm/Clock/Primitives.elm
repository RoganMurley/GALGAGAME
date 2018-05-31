module Clock.Primitives exposing (..)

import Clock.Meshes
import Clock.Shaders
import Clock.Types exposing (Uniforms, Vertex)
import Math.Vector2 exposing (Vec2)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings.Blend as WebGL


entity : Shader {} (Uniforms u) { vcoord : Vec2 } -> Mesh Vertex -> Uniforms u -> Entity
entity =
    WebGL.entityWith
        [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
        Clock.Shaders.vertex


quad : Shader {} (Uniforms u) { vcoord : Vec2 } -> Uniforms u -> Entity
quad fragment =
    entity
        fragment
        Clock.Meshes.quad


circle : Uniforms u -> Entity
circle =
    entity
        circleFragment
        Clock.Meshes.quad


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
            gl_FragColor = vec4(color, inner * outer);
        }

    |]


gear : Uniforms u -> Entity
gear =
    entity
        gearFragment
        Clock.Meshes.quad


gearFragment : Shader {} (Uniforms u) { vcoord : Vec2 }
gearFragment =
    [glsl|
        precision mediump float;

        uniform vec2 resolution;

        varying vec2 vcoord;

        void main ()
        {
            vec2 uv = vcoord;

            vec2 pos = vec2(.5) - uv;

            float r = length(pos) * 1.5;
            float a = atan(pos.y, pos.x);

            float f = smoothstep(-.5, 1., cos(a * 10.)) * .2 + .5;

            gl_FragColor = vec4(1. - smoothstep(f, f + .05, r));
        }

    |]