module Render.Int exposing (view)

import Game.Types exposing (Context)
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector3 exposing (Vec3, vec3)
import Render.Primitives
import Render.Shaders
import Texture.State as Texture
import WebGL


intToDigits : Int -> List Int
intToDigits int =
    if int > 0 then
        modBy 10 int :: intToDigits (int // 10)

    else
        []


view : Int -> { x : Float, y : Float, scale : Float, color : Vec3 } -> Context -> List WebGL.Entity
view num { x, y, scale, color } { w, h, textures } =
    let
        digits =
            List.reverse <| intToDigits num

        centring =
            scale * 1.5 * (toFloat <| List.length digits) * 0.5
    in
    List.concat <|
        List.indexedMap
            (\index digit ->
                Texture.with textures "numeroFontMap.png" <|
                    \texture ->
                        [ Render.Primitives.quad Render.Shaders.font
                            { rotation = makeRotate pi (vec3 0 0 1)
                            , scale = makeScale3 scale scale 1
                            , color = color
                            , pos = vec3 (x + toFloat index * scale * 1.5 - centring) y 0
                            , worldRot = makeRotate 0 (vec3 0 0 1)
                            , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                            , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                            , texture = texture
                            , digit = toFloat digit
                            }
                        ]
            )
            digits
