module Clock.Wave exposing (..)

import Animation.Types exposing (Anim(..))
import Clock.Primitives as Primitives
import Clock.Types exposing (Context)
import Clock.Uniforms exposing (uniforms)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Texture.State as Texture
import Util exposing (interpFloat)
import WebGL


view : Context -> List WebGL.Entity
view ({ w, h, radius, anim, textures } as ctx) =
    let
        progress =
            case anim of
                Just (Heal _) ->
                    1 - ctx.progress

                _ ->
                    ctx.progress

        scale =
            interpFloat progress 0 (3 * radius)
    in
        case Texture.load textures "noise" of
            Just texture ->
                case anim of
                    Just (Slash _ _) ->
                        [ Primitives.circle <|
                            uniforms ( floor w, floor h )
                                texture
                                (vec3 (w / 2) (h / 2) 0)
                                (makeScale3 scale scale 1)
                                (makeRotate 0 (vec3 0 0 1))
                                (vec3 1 0 0)
                        ]

                    Just (Bite _ _) ->
                        [ Primitives.circle <|
                            uniforms ( floor w, floor h )
                                texture
                                (vec3 (w / 2) (h / 2) 0)
                                (makeScale3 scale scale 1)
                                (makeRotate 0 (vec3 0 0 1))
                                (vec3 1 0 0)
                        ]

                    Just (Heal _) ->
                        [ Primitives.circle <|
                            uniforms ( floor w, floor h )
                                texture
                                (vec3 (w / 2) (h / 2) 0)
                                (makeScale3 scale scale 1)
                                (makeRotate 0 (vec3 0 0 1))
                                (vec3 0 1 0)
                        ]

                    _ ->
                        []

            Nothing ->
                []
