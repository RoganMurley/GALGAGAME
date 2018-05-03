module Clock.View exposing (view)

import Clock.Types exposing (Model)
import Html exposing (..)
import Html.Attributes exposing (..)
import Main.Messages as Main
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Clock.Meshes
import Clock.Shaders
import Clock.State exposing (uniforms)
import Raymarch.Types exposing (Params(..))
import Texture.State as Texture
import Texture.Types as Texture
import WebGL
import WebGL.Settings.Blend as WebGL


view : Params -> Model -> Texture.Model -> Html Main.Msg
view (Params _ ( w, h )) ({ time } as model) textures =
    let
        theta =
            time / 1000

        mTextures =
            Maybe.map2 (,)
                (Texture.load textures "wireframe-sword")
                (Texture.load textures "clock")

        points =
            Clock.State.clockFace 12 (vec3 0 0 0) 1 model

        locals =
            uniforms theta ( w, h )

        texEntity =
            WebGL.entityWith
                [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                Clock.Shaders.vertex
                Clock.Shaders.fragment
                Clock.Meshes.quad
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
                                texEntity <|
                                    locals sword pos rot (makeScale3 0.2 0.2 1)
                        in
                            List.concat
                                [ [ texEntity <|
                                        locals circle
                                            (vec3 0 1 0)
                                            (makeScale3 0.3 0.3 1)
                                            (makeRotate 0 <| vec3 0 0 1)
                                  ]
                                , List.map makeEntity points
                                , [ texEntity <|
                                        locals circle
                                            (vec3 0 0 0)
                                            (makeScale3 0.2 0.2 1)
                                            (makeRotate theta <| vec3 0 0 1)
                                  , texEntity <|
                                        locals circle
                                            (vec3 0 0 0)
                                            (makeScale3 0.7 0.7 1)
                                            (makeRotate 0 <| vec3 0 0 1)
                                  , texEntity <|
                                        locals circle
                                            (vec3 0 0 0)
                                            (makeScale3 1.55 1.55 1)
                                            (makeRotate -theta <| vec3 0 0 1)
                                  ]
                                ]

                    Nothing ->
                        []
                )
            ]
