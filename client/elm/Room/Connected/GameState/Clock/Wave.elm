module Clock.Wave exposing (..)

import Animation.Types exposing (Anim(..))
import Clock.Primitives as Primitives
import Clock.Types exposing (Context)
import Colour
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Util exposing (interpFloat)
import WebGL


view : Context -> List WebGL.Entity
view ({ w, h, radius, anim } as ctx) =
    let
        progress =
            case anim of
                Heal _ ->
                    1 - ctx.progress

                _ ->
                    ctx.progress

        scale =
            interpFloat progress 0 (3 * radius)
    in
        case anim of
            Slash _ _ ->
                [ Primitives.circle
                    { rotation = makeRotate 0 (vec3 0 0 1)
                    , scale = makeScale3 scale scale 1
                    , color = Colour.red
                    , worldPos = vec3 (w / 2) (h / 2) 0
                    , worldRot = makeRotate 0 (vec3 0 0 1)
                    , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                    }
                ]

            Bite _ _ ->
                [ Primitives.circle
                    { rotation = makeRotate 0 (vec3 0 0 1)
                    , scale = makeScale3 scale scale 1
                    , color = Colour.red
                    , worldPos = vec3 (w / 2) (h / 2) 0
                    , worldRot = makeRotate 0 (vec3 0 0 1)
                    , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                    }
                ]

            Heal _ ->
                [ Primitives.circle
                    { rotation = makeRotate 0 (vec3 0 0 1)
                    , scale = makeScale3 scale scale 1
                    , color = Colour.green
                    , worldPos = vec3 (w / 2) (h / 2) 0
                    , worldRot = makeRotate 0 (vec3 0 0 1)
                    , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                    }
                ]

            _ ->
                []
