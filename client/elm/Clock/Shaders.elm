module Clock.Shaders exposing (..)

import Clock.Types exposing (Uniforms, Vertex)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Matrix4 exposing (Mat4)
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


null : Shader {} a {}
null =
    [glsl|
        void main ()
        {
            discard;
        }
    |]


vertex : Shader Vertex { u | perspective : Mat4, camera : Mat4, worldPos : Vec3, rotation : Mat4, worldRot : Mat4 } { vcoord : Vec2 }
vertex =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec2 coord;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        uniform vec3 worldPos;
        uniform mat4 worldRot;

        varying vec2 vcoord;

        void main () {
            gl_Position = perspective * camera * worldRot * (vec4(worldPos, 1.0) + rotation * vec4(position, 1.0));
            vcoord = coord.xy;
        }

    |]
