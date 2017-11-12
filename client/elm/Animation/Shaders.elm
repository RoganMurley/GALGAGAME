module Animation.Shaders exposing (..)

import Raymarch.Types exposing (Uniforms, Vertex)
import WebGL exposing (Shader)


slashA : Shader {} Uniforms {}
slashA =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2 resolution;

        void main ()
        {
            vec2 uv = gl_FragCoord.xy / resolution.xy;

            float radius = 0.3;
            if (time > .8) {
                discard;
                return;
            }

            gl_FragColor = vec4(1., 1., 1., 0.);
            if ((uv.x - .5) * (uv.x - .5) + (uv.y - .5) * (uv.y - .5) < radius * radius) {
                if ((uv.x - .4) * (uv.x - .5) + (uv.y - .4) * (uv.y - .4) > radius * radius * (1.8 - .6 * abs(sin(4. * time)))) {
                    if (uv.x < abs(sin(time) * 3.)) {
                        gl_FragColor = vec4(1., .02, .02, 1.);
                        return;
                    }
                }
            }
            discard;
        }

    |]


slashB : Shader {} Uniforms {}
slashB =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2 resolution;

        void main ()
        {
            vec2 uv = gl_FragCoord.xy / resolution.xy;

            float radius = 0.3;
            if (time > .8) {
                discard;
                return;
            }

            gl_FragColor = vec4(1., 1., 1., 0.);
            if ((uv.x - .5) * (uv.x - .5) + (uv.y - .5) * (uv.y - .5) < radius * radius) {
                if ((uv.x - .6) * (uv.x - .5) + (uv.y - .6) * (uv.y - .6) > radius * radius * (1.8 - .6 * abs(sin(4. * time)))) {
                    if ((1. - uv.x) < abs(sin(time) * 3.)) {
                        gl_FragColor = vec4(1., .02, .02, 1.);
                        return;
                    }
                }
            }
            discard;
        }

    |]


obliterate : Shader {} Uniforms {}
obliterate =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2 resolution;

        void main ()
        {
            vec2 uv = gl_FragCoord.xy / resolution.xy;

            float radius = 0.3;
            if (time > .8) {
                discard;
                return;
            }

            gl_FragColor = vec4(1., 1., 1., 0.);
            if ((uv.x - .5) * (uv.x - .5) + (uv.y - .5) * (uv.y - .5) < radius * radius) {
                if ((uv.x - .4) * (uv.x - .5) + (uv.y - .4) * (uv.y - .4) > radius * radius * (1.8 - .6 * abs(sin(4. * time)))) {
                    if (uv.x < abs(sin(time) * 3.)) {
                        gl_FragColor = vec4(1., .02, .02, 1.);
                        return;
                    }
                }
            }
            discard;
        }

    |]


null : Shader {} Uniforms {}
null =
    [glsl|
        void main ()
        {
            discard;
        }
    |]
