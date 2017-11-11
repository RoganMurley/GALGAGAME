module Animation.View exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Raymarch.Meshes exposing (quadMesh)
import Raymarch.State exposing (uniforms)
import Raymarch.Types exposing (Params(..))
import Animation.Shaders as Animation
import Raymarch.Shaders as Raymarch
import WebGL


view : Params -> Float -> Html msg
view (Params theta ( w, h )) resTheta =
    let
        time =
            theta / 1000

        resTime =
            resTheta / 1000

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
            [ WebGL.entityWith
                []
                Raymarch.vertexShader
                Raymarch.fragmentShader
                quadMesh
                (uniforms time ( w, h ))
            , WebGL.entityWith
                []
                Animation.vertexShader
                Animation.fragmentShader
                quadMesh
                (uniforms resTime ( w // downscale, h // downscale ))
            ]
