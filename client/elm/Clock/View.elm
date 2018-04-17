module Clock.View exposing (view)

import Clock.Types exposing (Model)
import Html exposing (..)
import Html.Attributes exposing (..)
import Main.Messages as Main
import Raymarch.Meshes exposing (quadMesh)
import Clock.Shaders
import Clock.State exposing (uniforms)
import Raymarch.Types exposing (Params(..))
import Texture.State as Texture
import Texture.Types as Texture
import WebGL
import WebGL.Settings.Blend as WebGL


view : Params -> Model -> Texture.Model -> Html Main.Msg
view (Params _ ( w, h )) { time } textures =
    let
        theta =
            time / 1000

        downscale =
            1

        mTexture =
            Texture.load textures "clock"
    in
        div [ class "clock" ]
            (case mTexture of
                Just texture ->
                    [ WebGL.toHtml
                        [ width (w // downscale)
                        , height (h // downscale)
                        , class "raymarch-canvas"
                        ]
                        [ WebGL.entityWith
                            [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                            Clock.Shaders.vertex
                            Clock.Shaders.fragment
                            quadMesh
                            (uniforms theta ( w, h ) texture)
                        ]
                    ]

                Nothing ->
                    [ div [ class "error" ] [ text "error loading texture" ] ]
            )
