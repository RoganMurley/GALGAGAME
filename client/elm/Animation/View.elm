module Animation.View exposing (..)

import Animation.Types exposing (Anim)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Raymarch.Meshes exposing (quadMesh)
import Raymarch.State as Raymarch
import Raymarch.Types exposing (Params(..))
import Animation.State exposing (animToFragmentShader, animToTexture, getWhichPlayer, uniforms)
import Animation.Shaders
import Raymarch.Shaders
import Texture.Types as Texture
import WhichPlayer.Types exposing (WhichPlayer(..))
import WebGL
import WebGL.Settings.Blend as WebGL
import WebGL.Texture exposing (Texture)


view : Params -> Float -> Maybe Anim -> Texture.Model -> Html msg
view (Params theta ( w, h )) resTheta anim textures =
    let
        time =
            theta / 1000

        resTime =
            resTheta / 1000

        downscale =
            5

        which : Maybe WhichPlayer
        which =
            Maybe.map getWhichPlayer anim

        texture : Texture
        texture =
            animToTexture anim textures
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
                    Raymarch.Shaders.vertexShader
                    Raymarch.Shaders.fragmentShader
                    quadMesh
                    (Raymarch.uniforms time ( w, h ))
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
                    Animation.Shaders.vertex
                    (animToFragmentShader anim)
                    quadMesh
                    (uniforms resTime which ( w, h ) texture)
                ]
            ]
