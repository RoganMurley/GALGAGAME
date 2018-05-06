module Clock.View exposing (view)

import Clock.Primitives as Primitives
import Clock.Shaders
import Clock.State exposing (uniforms)
import Clock.Types exposing (Model)
import Ease
import Html exposing (..)
import Html.Attributes exposing (..)
import Main.Messages as Main
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Mouse
import Raymarch.Types exposing (Params(..))
import Texture.State as Texture
import Texture.Types as Texture
import WebGL


view : Params -> Mouse.Position -> Model -> Texture.Model -> Html Main.Msg
view (Params _ ( w, h )) mouse ({ time, maxTick } as model) textures =
    let
        theta =
            time / 1000

        mTextures =
            Maybe.map2 (,)
                (Texture.load textures "wireframe-sword")
                (Texture.load textures "clock")

        points =
            Clock.State.clockFace 12 (vec3 (toFloat w / 2) (toFloat h / 2) 0) ((0.65 * radius)) model

        locals =
            uniforms theta ( w, h )

        radius =
            0.8 * (toFloat h / 2)
    in
        div [ class "clock" ]
            [ WebGL.toHtml
                [ width w
                , height h
                , class "raymarch-canvas"
                ]
                (case mTextures of
                    Just ( sword, circle ) ->
                        let
                            makeEntity ( pos, rot ) =
                                Primitives.quad Clock.Shaders.fragment <|
                                    locals sword pos rot (makeScale3 (0.13 * radius) (0.13 * radius) 1)

                            handView : Int -> List WebGL.Entity
                            handView n =
                                let
                                    ( width, height, spacing ) =
                                        ( 0.1 * radius, 0.1 * radius, 35.0 )

                                    origin =
                                        vec3
                                            ((toFloat w / 2) - (0.5 * (width + spacing) * (toFloat n)))
                                            (toFloat h - height)
                                            0

                                    entity : Int -> WebGL.Entity
                                    entity i =
                                        Primitives.quad Clock.Shaders.fragment <|
                                            locals sword
                                                (Math.Vector3.add origin <|
                                                    vec3 ((toFloat i) * (width + spacing)) 0 0
                                                )
                                                (makeScale3 width height 1)
                                                (makeRotate pi <| vec3 0 0 1)
                                in
                                    List.map entity (List.range 0 n)

                            otherHandView : Int -> List WebGL.Entity
                            otherHandView n =
                                let
                                    ( width, height, spacing ) =
                                        ( 0.1 * radius, 0.1 * radius, 35.0 )

                                    origin =
                                        vec3
                                            ((toFloat w / 2) - (0.5 * (width + spacing) * (toFloat n)))
                                            height
                                            0

                                    entity : Int -> WebGL.Entity
                                    entity i =
                                        Primitives.quad Clock.Shaders.fragment <|
                                            locals sword
                                                (Math.Vector3.add origin <|
                                                    vec3 ((toFloat i) * (width + spacing)) 0 0
                                                )
                                                (makeScale3 width height 1)
                                                (makeRotate 0 <| vec3 0 0 1)
                                in
                                    List.map entity (List.range 0 n)
                        in
                            List.concat
                                [ [ Primitives.circle <|
                                        locals circle
                                            (vec3 (toFloat w / 2) (toFloat h / 2) 0)
                                            (makeScale3 (0.8 * radius) (0.8 * radius) 1)
                                            (makeRotate 0 <| vec3 0 0 1)
                                  ]
                                , List.map makeEntity points
                                , [ Primitives.circle <|
                                        locals circle
                                            (vec3 (toFloat w / 2) (toFloat h / 2) 0)
                                            (makeScale3 (0.5 * radius) (0.5 * radius) 1)
                                            (makeRotate theta <| vec3 0 0 1)
                                  , Primitives.circle <|
                                        locals circle
                                            (vec3 (toFloat w / 2) ((toFloat h / 2) - (0.65 * radius)) 0)
                                            (makeScale3 (0.15 * radius) (0.15 * radius) 1)
                                            (makeRotate 0 <| vec3 0 0 1)
                                  ]
                                , [ Primitives.gear <|
                                        locals circle
                                            (vec3
                                                ((toFloat w / 2) + radius * 0.1)
                                                ((toFloat h / 2))
                                                0
                                            )
                                            (makeScale3 (0.1 * radius) (0.1 * radius) 1)
                                            (makeRotate (2 * pi * 0.09 * (Ease.inQuad <| time / maxTick)) <|
                                                vec3 0 0 1
                                            )
                                  , Primitives.gear <|
                                        locals circle
                                            (vec3
                                                ((toFloat w / 2) - radius * 0.065)
                                                ((toFloat h / 2) - radius * 0.02)
                                                0
                                            )
                                            (makeScale3 (0.1 * radius) (0.1 * radius) 1)
                                            (makeRotate -(2 * pi * 0.09 * (Ease.inQuad <| time / maxTick)) <|
                                                vec3 0 0 1
                                            )
                                  ]
                                , [ Primitives.quad Clock.Shaders.fragment <|
                                        locals sword
                                            (vec3
                                                ((toFloat mouse.x))
                                                ((toFloat mouse.y))
                                                0
                                            )
                                            (makeScale3 (0.1 * radius) (0.1 * radius) 1)
                                            (makeRotate pi <| vec3 0 0 1)
                                  ]
                                , handView 5
                                , otherHandView 4
                                ]

                    Nothing ->
                        []
                )
            ]
