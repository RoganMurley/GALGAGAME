module Animation.View exposing (..)

import Animation.Types exposing (Anim)
import Html exposing (Html)
import Html.Attributes exposing (class, width, height, style)
import Raymarch.Meshes exposing (quadMesh)
import Raymarch.State as Raymarch
import Raymarch.Types exposing (Params(..))
import Animation.Types exposing (FragShader(..))
import Animation.State exposing (animToFragShader, animToTexture, getWhichPlayer, uniforms)
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

        animEntity : WebGL.Entity
        animEntity =
            case anim of
                Just a ->
                    let
                        texture : Maybe Texture
                        texture =
                            animToTexture a textures

                        shader : FragShader
                        shader =
                            animToFragShader a

                        myUniforms =
                            uniforms resTime which ( w, h )
                    in
                        case shader of
                            BaseShader s ->
                                WebGL.entityWith
                                    [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                                    Animation.Shaders.vertex
                                    s
                                    quadMesh
                                    myUniforms

                            TexturedShader s ->
                                case texture of
                                    Just tex ->
                                        let
                                            { time, resolution, flipper } =
                                                myUniforms

                                            newUniforms : Animation.Types.Uniforms (Animation.Types.Textured {})
                                            newUniforms =
                                                { time = time
                                                , resolution = resolution
                                                , flipper = flipper
                                                , texture = tex
                                                }
                                        in
                                            WebGL.entityWith
                                                [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                                                Animation.Shaders.vertex
                                                s
                                                quadMesh
                                                newUniforms

                                    Nothing ->
                                        WebGL.entityWith
                                            [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                                            Animation.Shaders.vertex
                                            Animation.Shaders.null
                                            quadMesh
                                            myUniforms

                Nothing ->
                    WebGL.entityWith
                        [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                        Animation.Shaders.vertex
                        Animation.Shaders.null
                        quadMesh
                        (uniforms resTime which ( w, h ))
    in
        Html.div []
            [ WebGL.toHtml
                [ width (w // downscale)
                , height (h // downscale)
                , class "raymarch-canvas"
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
                , class "animation-canvas"
                ]
                [ animEntity ]
            ]
