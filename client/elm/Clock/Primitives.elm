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

        uniform mat4 scale;

        varying vec2 vcoord;

        void main ()
        {
            float scaleX = length(vec4(scale[0][0], scale[0][1], scale[0][2], scale[0][3]));
            float thickness = 0.01*scaleX;

            float dist = length(vcoord - vec2(.5, .5));
            if (dist < 0.5 && dist > 0.5 - thickness) {
                gl_FragColor = vec4(1., 1., 1., 1.);
            } else {
                gl_FragColor = vec4(0, 0, 0, 0);
            }
        }

    |]
