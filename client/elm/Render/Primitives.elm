module Render.Primitives exposing (..)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Render.Meshes
import Render.Shaders
import Render.Types exposing (Vertex)
import Render.Uniforms exposing (Uniforms)
import WebGL exposing (Entity, Mesh, Shader, Texture)
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


circle : Uniforms {} -> Entity
circle =
    entity
        Render.Shaders.circleFragment
        Render.Meshes.quad


fullCircle : Uniforms { mag : Float } -> Entity
fullCircle =
    entity
        Render.Shaders.fullCircleFragment
        Render.Meshes.quad


roundedBox : Uniforms {} -> Entity
roundedBox =
    entity
        Render.Shaders.roundedBoxFragment
        Render.Meshes.quad


roundedBoxDisintegrate : Uniforms { texture : Texture, time : Float } -> Entity
roundedBoxDisintegrate =
    entity
        Render.Shaders.roundedBoxDisintegrate
        Render.Meshes.quad


roundedBoxTransmute : Uniforms { time : Float, finalColor : Vec3 } -> Entity
roundedBoxTransmute =
    entity
        Render.Shaders.roundedBoxTransmute
        Render.Meshes.quad
