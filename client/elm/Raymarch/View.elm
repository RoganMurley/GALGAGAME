module Raymarch.View exposing (..)

import Animation.Shaders
import Html exposing (Html)
import Html.Attributes exposing (class, width, height, style)
import Raymarch.Meshes exposing (quadMesh)
import Raymarch.Types exposing (Params(..))
import Raymarch.Shaders exposing (fragmentShader, vertexShader)
import Raymarch.State exposing (uniforms)
import WebGL


view : Params -> Html msg
view (Params theta ( w, h )) =
    let
        time =
            theta / 1000

        downscale =
            5
    in
        Html.div []
            [ WebGL.toHtml
                [ width (w // downscale)
                , height (h // downscale)
                , class "raymarch-canvas"
                ]
                [ WebGL.entityWith []
                    vertexShader
                    fragmentShader
                    quadMesh
                    (uniforms time ( w, h ))
                ]
            , WebGL.toHtml
                [ width w
                , height h
                , class "animation-canvas"
                ]
                [ WebGL.entityWith
                    []
                    vertexShader
                    Animation.Shaders.null
                    quadMesh
                    (uniforms time ( w, h ))
                ]
            ]
