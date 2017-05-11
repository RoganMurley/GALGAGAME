module Raymarch.View exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Main.Messages exposing (Msg)
import Raymarch.Meshes exposing (quadMesh)
import Raymarch.Types exposing (Params(..))
import Raymarch.Shaders exposing (fragmentShader, vertexShader)
import Raymarch.State exposing (uniforms)
import WebGL


view : Params -> Html Msg
view (Params theta ( w, h )) =
    let
        time =
            theta / 1000

        downscale =
            5
    in
        WebGL.toHtml
            [ width (w // downscale)
            , height (h // downscale)
            , style
                [ ( "position", "absolute" )
                , ( "top", "0" )
                , ( "z-index", "-999" )
                , ( "width", "100%" )
                , ( "height", "100%" )
                ]
            ]
            [ WebGL.entityWith []
                vertexShader
                fragmentShader
                quadMesh
                (uniforms time ( w, h ))
            ]
