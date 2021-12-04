module Render.Primitives exposing (circle, donut, entity, fullCircle, quad)

import Math.Vector2 exposing (Vec2)
import Render.Meshes
import Render.Shaders
import Render.Types exposing (Vertex)
import Render.Uniforms exposing (Uniforms)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings.Blend as WebGL


entity : Shader {} (Uniforms u) { vcoord : Vec2 } -> Mesh Vertex -> Uniforms u -> Entity
entity =
    WebGL.entityWith
        [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
        Render.Shaders.vertex


quad : Shader {} (Uniforms u) { vcoord : Vec2 } -> Uniforms u -> Entity
quad fragment =
    entity
        fragment
        Render.Meshes.quad


circle : Uniforms { mag : Float } -> Entity
circle =
    entity
        Render.Shaders.circleFragment
        Render.Meshes.quad


fullCircle : Uniforms { mag : Float } -> Entity
fullCircle =
    entity
        Render.Shaders.fullCircleFragment
        Render.Meshes.quad


donut : Uniforms { mag : Float } -> Entity
donut =
    entity
        Render.Shaders.donutFragment
        Render.Meshes.quad
