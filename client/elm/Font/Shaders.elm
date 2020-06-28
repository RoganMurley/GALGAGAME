module Font.Shaders exposing (char)

import Math.Vector2 exposing (Vec2)
import Render.Uniforms exposing (Uniforms)
import WebGL exposing (Shader)
import WebGL.Texture exposing (Texture)


char : Shader {} (Uniforms { texture : Texture, x : Float, y : Float, width : Float, height : Float, textureHeight : Float, textureWidth : Float }) { vcoord : Vec2 }
char =
    [glsl|
        precision mediump float;

        uniform vec3 color;
        uniform sampler2D texture;
        uniform float x;
        uniform float y;
        uniform float width;
        uniform float height;
        uniform float textureWidth;
        uniform float textureHeight;

        varying vec2 vcoord;

        void main ()
        {
            vec2 uv = vec2(
                x / textureWidth + vcoord.x * width / textureWidth,
                1. - (height + y) / textureHeight + vcoord.y * height / textureHeight
            );
            vec4 sample = texture2D(texture, uv);
            gl_FragColor = vec4(color, step(1. - sample.z, 0.99));
            //gl_FragColor = vec4(color, step(sample.z, 0.99));
        }

    |]
