module Raymarch.Shaders exposing (fragmentShader, vertexShader)

import Raymarch.Types exposing (Uniforms, Vertex)
import WebGL exposing (Shader)


vertexShader : Shader Vertex Uniforms {}
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;

        uniform float time;

        void main () {
            gl_Position = vec4(position, 1.0);
        }

    |]


fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2 resolution;

        const float ITERATIONS = 16.0;
        const float VIEW_DISTANCE = 8.0;
        const float CLOUD_DENSITY = 0.65;
        const float OCTAVES = 4.0;

        const vec4 SKY_COLOR = vec4(0.012, 0.0178, 0.045, 1.0);
        const vec4 CLOUD_COLOR = vec4(0.8, 0.8, 0.8, 0.7);

        float hash(vec3 p)
        {
            p  = fract( p*0.3183099+.1 );
        	p *= 17.0;
            return fract( p.x*p.y*p.z*(p.x+p.y+p.z) );
        }

        float noise( in vec3 x )
        {
            vec3 p = floor(x);
            vec3 f = fract(x);
            f = f*f*(3.0-2.0*f);

            return mix(mix(mix( hash(p+vec3(0,0,0)),
                                hash(p+vec3(1,0,0)),f.x),
                           mix( hash(p+vec3(0,1,0)),
                                hash(p+vec3(1,1,0)),f.x),f.y),
                       mix(mix( hash(p+vec3(0,0,1)),
                                hash(p+vec3(1,0,1)),f.x),
                           mix( hash(p+vec3(0,1,1)),
                                hash(p+vec3(1,1,1)),f.x),f.y),f.z);
        }

        float fbm(vec3 pos)
        {
            float f = 0.0;
            for (float i = 0.0; i < OCTAVES; i++)
            {
                f += noise(pos) / pow(2.0, i + 1.0);
                pos *= 2.01;
            }
            f /= 1.0 - 1.0 / pow(2.0, OCTAVES + 1.0);
            return f;
        }

        void main ()
        {
          vec2 uv = gl_FragCoord.xy / resolution.xy;
          uv = uv * 2.0 - 1.0;
          uv.x *= resolution.x / resolution.y;

          vec3 ray = normalize(vec3(uv, 1.0));
          vec3 pos = vec3(0.0, 0.0, -1000.0-time*0.2);

          float density = 0.1;

          // Raymarching
          for (float i = 0.0; i < ITERATIONS; i++)
          {
              float f = i / ITERATIONS;
              float alpha = smoothstep(0.0, ITERATIONS * 0.1, i) * (1.0 - f) * (1.0 - f);

              float denseClouds = smoothstep(CLOUD_DENSITY, 0.75, fbm(pos));
              density += denseClouds * alpha;

              pos += ray * f * VIEW_DISTANCE;
          }

          vec3 color = SKY_COLOR.rgb + (CLOUD_COLOR.rgb - 0.5) * (density / ITERATIONS) * 20.0 * CLOUD_COLOR.a;

          gl_FragColor = vec4(color, 1.0);
        }

    |]
