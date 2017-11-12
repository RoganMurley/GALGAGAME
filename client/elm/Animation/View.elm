module Animation.View exposing (..)

import Card.Types as Card
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Raymarch.Meshes exposing (quadMesh)
import Raymarch.State exposing (uniforms)
import Raymarch.Types exposing (Params(..))
import Animation.State exposing (animToFragmentShader)
import Raymarch.Shaders as Raymarch
import Model.Types exposing (WhichPlayer(..))
import WebGL
import WebGL.Settings.Blend as WebGL


view : Params -> Float -> Maybe ( WhichPlayer, Card.Anim ) -> Html msg
view (Params theta ( w, h )) resTheta animParams =
    let
        time =
            theta / 1000

        resTime =
            resTheta / 1000

        downscale =
            5
    in
        Html.div []
            [ WebGL.toHtml
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
                    [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                    Raymarch.vertexShader
                    Raymarch.fragmentShader
                    quadMesh
                    (uniforms time ( w, h ))
                ]
            , WebGL.toHtml
                [ width w
                , height h
                , style
                    [ ( "position", "absolute" )
                    , ( "top", "0" )
                    , ( "z-index", "999" )
                    , ( "width", "100%" )
                    , ( "height", "100%" )
                    , ( "pointer-events", "none" )
                    ]
                ]
                [ WebGL.entityWith
                    [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                    Raymarch.vertexShader
                    (animToFragmentShader animParams)
                    quadMesh
                    (uniforms resTime ( w, h ))
                ]
            ]
