module Clock.View exposing (view)

import Clock.Types exposing (Model)
import Html exposing (..)
import Html.Attributes exposing (..)
import Main.Messages as Main
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
view (Params _ ( w, h )) { time } textures =
    let
        theta =
            time / 1000

        downscale =
            1

        mTexture =
            Texture.load textures "sword"

        positions =
            Clock.State.clockFace 12 (vec3 0 0 0) 1
    in
        div [ class "clock" ]
            [ WebGL.toHtml
                [ width (w // downscale)
                , height (h // downscale)
                , class "raymarch-canvas"
                ]
                (case mTexture of
                    Just texture ->
                        let
                            makeEntity index ( pos, rot ) =
                                WebGL.entityWith
                                    [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                                    Clock.Shaders.vertex
                                    Clock.Shaders.fragment
                                    mesh
                                    (uniforms theta ( w, h ) texture pos rot)
                        in
                            List.indexedMap makeEntity positions

                    Nothing ->
                        []
                )
            ]


mesh : WebGL.Mesh Clock.Types.Vertex
mesh =
    Clock.Meshes.quad 0.2
