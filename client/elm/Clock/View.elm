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
            Texture.load textures "clock"

        positions =
            Clock.State.clockFace 12 (vec3 0 0 0) 1.0
    in
        div [ class "clock" ]
            (case mTexture of
                Just texture ->
                    let
                        makeEntity pos =
                            WebGL.entityWith
                                [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                                Clock.Shaders.vertex
                                Clock.Shaders.fragment
                                (Clock.Meshes.quad pos 0.2)
                                (uniforms theta ( w, h ) texture)

                        entities =
                            List.map makeEntity positions
                    in
                        [ WebGL.toHtml
                            [ width (w // downscale)
                            , height (h // downscale)
                            , class "raymarch-canvas"
                            ]
                            entities
                          -- [ WebGL.entityWith
                          --     [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                          --     Clock.Shaders.vertex
                          --     Clock.Shaders.fragment
                          --     (Clock.Meshes.quad (vec3 -0.5 -0.5 0) 0.2)
                          --     (uniforms theta ( w, h ) texture)
                          -- , WebGL.entityWith
                          --     [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                          --     Clock.Shaders.vertex
                          --     Clock.Shaders.fragment
                          --     (Clock.Meshes.quad (vec3 0.5 0.5 -0.3) 0.3)
                          --     (uniforms theta ( w, h ) texture)
                          -- , WebGL.entityWith
                          --     [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                          --     Clock.Shaders.vertex
                          --     Clock.Shaders.fragment
                          --     (Clock.Meshes.quad (vec3 -0.5 0.5 0.7) 0.1)
                          --     (uniforms theta ( w, h ) texture)
                          -- , WebGL.entityWith
                          --     [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                          --     Clock.Shaders.vertex
                          --     Clock.Shaders.fragment
                          --     (Clock.Meshes.quad (vec3 0.5 -0.5 0.1) 0.4)
                          --     (uniforms theta ( w, h ) texture)
                          -- , WebGL.entityWith
                          --     [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                          --     Clock.Shaders.vertex
                          --     Clock.Shaders.matte
                          --     (Clock.Meshes.quad (vec3 theta 0.0 (-10.0 * theta)) 1.0)
                          --     (Clock.State.cameraUniforms (vec3 1.0 0.0 0.0))
                          -- ]
                        ]

                Nothing ->
                    [ div [ class "error" ] [ text "error loading texture" ] ]
            )
