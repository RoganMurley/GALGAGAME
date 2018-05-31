module Clock.View exposing (view)

import Animation.Types exposing (Anim(..))
import Clock.Hand exposing (handView, otherHandView)
import Clock.Primitives as Primitives
import Clock.Shaders
import Clock.Stack
import Clock.State exposing (animToResTickMax, uniforms)
import Clock.Types exposing (ClockParams, Model)
import Clock.Wave
import Ease
import Html exposing (..)
import Html.Attributes exposing (..)
import Main.Messages as Main
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (Vec3, vec3)
import Mouse
import Raymarch.Types exposing (Params(..))
import Resolvable.State exposing (activeAnim, activeModel)
import Texture.State as Texture
import Texture.Types as Texture
import WebGL


view : Params -> Mouse.Position -> Model -> Texture.Model -> Html Main.Msg
view (Params _ ( w, h )) mouse { res } textures =
    let
        mTextures =
            Maybe.map2 (,)
                (Texture.load textures "wireframe-sword")
                (Texture.load textures "clock")

        locals =
            uniforms 0 ( w, h )

        radius =
            case anim of
                Just (GameStart _) ->
                    0.8 * (toFloat h / 2) * (Ease.outQuint <| res.tick / maxTick)

                otherwise ->
                    0.8 * (toFloat h / 2)

        anim =
            activeAnim res

        model =
            activeModel res

        maxTick =
            animToResTickMax anim

        rotateProgress =
            case anim of
                Just (Rotate _) ->
                    Ease.inQuad <| res.tick / maxTick

                Just (Play _ _ _) ->
                    1 - (Ease.outQuad <| res.tick / maxTick)

                otherwise ->
                    0

        z =
            0

        params : ClockParams
        params =
            { w = toFloat w
            , h = toFloat h
            , radius = radius
            }

        resInfo =
            Just ( res.tick, anim )
    in
        div [ class "clock" ]
            [ WebGL.toHtml
                [ width w
                , height h
                , class "raymarch-canvas"
                ]
                (case mTextures of
                    Just ( sword, circle ) ->
                        List.concat
                            [ [ Primitives.circle <|
                                    locals circle
                                        (vec3 (toFloat w / 2) (toFloat h / 2) z)
                                        (makeScale3 (0.8 * radius) (0.8 * radius) 1)
                                        (makeRotate 0 <| vec3 0 0 1)
                                        (vec3 1 1 1)
                              ]
                            , [ Primitives.circle <|
                                    locals circle
                                        (vec3 (toFloat w / 2) (toFloat h / 2) z)
                                        (makeScale3 (0.5 * radius) (0.5 * radius) 1)
                                        (makeRotate 0 <| vec3 0 0 1)
                                        (vec3 1 1 1)
                              , Primitives.circle <|
                                    locals circle
                                        (vec3 (toFloat w / 2) ((toFloat h / 2) - (0.615 * radius)) z)
                                        (makeScale3 (0.13 * radius) (0.13 * radius) 1)
                                        (makeRotate 0 <| vec3 0 0 1)
                                        (vec3 1 1 1)
                              ]
                            , [ Primitives.gear <|
                                    locals circle
                                        (vec3
                                            ((toFloat w / 2) + radius * 0.1)
                                            ((toFloat h / 2))
                                            z
                                        )
                                        (makeScale3 (0.1 * radius) (0.1 * radius) 1)
                                        (makeRotate (2 * pi * 0.09 * rotateProgress) <|
                                            vec3 0 0 1
                                        )
                                        (vec3 1 1 1)
                              , Primitives.gear <|
                                    locals circle
                                        (vec3
                                            ((toFloat w / 2) - radius * 0.065)
                                            ((toFloat h / 2) - radius * 0.02)
                                            z
                                        )
                                        (makeScale3 (0.1 * radius) (0.1 * radius) 1)
                                        (makeRotate -(2 * pi * 0.09 * rotateProgress) <|
                                            vec3 0 0 1
                                        )
                                        (vec3 1 1 1)
                              ]
                            , [ Primitives.quad Clock.Shaders.fragment <|
                                    locals sword
                                        (vec3
                                            ((toFloat mouse.x))
                                            ((toFloat mouse.y))
                                            z
                                        )
                                        (makeScale3 (0.1 * radius) (0.1 * radius) 1)
                                        (makeRotate pi <| vec3 0 0 1)
                                        (vec3 1 1 1)
                              ]
                            , Clock.Stack.view params model.stack resInfo sword
                            , handView params model.hand resInfo sword
                            , otherHandView params model.otherHand resInfo sword
                            , Clock.Wave.view params resInfo sword
                            ]

                    Nothing ->
                        []
                )
            ]
