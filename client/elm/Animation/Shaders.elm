module Animation.Shaders exposing (..)

import Animation.Types exposing (Uniforms, Textured, Vertex)
import WebGL exposing (Shader)


slash : Shader {} (Uniforms {}) {}
slash =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2 resolution;
        uniform float flipper;

        void main ()
        {
            vec2 uv = gl_FragCoord.xy / resolution.xy;
            uv = abs(vec2(flipper, flipper) - uv);

            float radius = 0.3;

            gl_FragColor = vec4(1., 1., 1., 0.);
            if ((uv.x - .5) * (uv.x - .5) + (uv.y - .5) * (uv.y - .5) < radius * radius) {
                if ((uv.x - .4) * (uv.x - .5) + (uv.y - .4) * (uv.y - .4) > radius * radius * (1.8 - .6 * abs(sin(4. * time)))) {
                    if (uv.x < abs(sin(time) * 3.)) {
                        gl_FragColor = vec4(1., .02, .02, 1.);
                    }
                }
            }
        }

    |]


heal : Shader {} (Uniforms {}) {}
heal =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2 resolution;
        uniform float flipper;

        void main ()
        {
            vec2 uv = gl_FragCoord.xy / resolution.xy;
            uv = abs(vec2(flipper, flipper) - uv);

            float intensity = cos(time + 0.5) * time - abs(1. - uv.y);
            gl_FragColor = vec4(0., 1., 0., intensity);
        }

    |]


obliterate : Shader {} (Uniforms {}) {}
obliterate =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2 resolution;
        uniform float flipper;

        void main ()
        {
            vec2 uv = gl_FragCoord.xy / resolution.xy;
            uv = abs(vec2(flipper, flipper) - uv);

            float d = distance(uv, vec2(uv.x, 0.5));
            gl_FragColor = vec4(1., 1., 1., sin(time) * d);

        }

    |]


overdraw : Shader {} (Uniforms (Textured {})) {}
overdraw =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2 resolution;
        uniform float flipper;
        uniform sampler2D texture;

        void main ()
        {
            vec2 uv = gl_FragCoord.xy / resolution.xy;
            uv = abs(vec2(flipper, flipper) - uv);

            gl_FragColor = texture2D(texture, uv);
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


vertex : Shader Vertex a {}
vertex =
    [glsl|
        precision mediump float;

        attribute vec3 position;

        void main () {
            gl_Position = vec4(position, 1.);
        }

    |]
