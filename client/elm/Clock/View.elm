module Clock.View exposing (view)

import Animation.Types exposing (Anim(..))
import Animation.State exposing (animToResTickMax)
import Clock.Primitives as Primitives
import Clock.Shaders
import Clock.State exposing (uniforms)
import Clock.Types exposing (Model)
import Ease
import Hand.Types exposing (Hand)
import Html exposing (..)
import Html.Attributes exposing (..)
import Main.Messages as Main
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (Vec3, vec3)
import Mouse
import WhichPlayer.Types exposing (WhichPlayer(..))
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

                otherwise ->
                    0

        z =
            0
    in
        div [ class "clock" ]
            [ WebGL.toHtml
                [ width w
                , height h
                , class "raymarch-canvas"
                ]
                (case mTextures of
                    Just ( sword, circle ) ->
                        let
                            stackView : List WebGL.Entity
                            stackView =
                                let
                                    makeCard ( pos, rot ) =
                                        Primitives.quad Clock.Shaders.fragment <|
                                            locals sword pos rot (makeScale3 (0.13 * radius) (0.13 * radius) 1)

                                    stackLen =
                                        List.length model.stack

                                    points =
                                        Clock.State.clockFace stackLen (vec3 (toFloat w / 2) (toFloat h / 2) 0) ((0.65 * radius)) rotateProgress
                                in
                                    case anim of
                                        Nothing ->
                                            []

                                        otherwise ->
                                            List.map makeCard points

                            handView : Hand -> List WebGL.Entity
                            handView finalHand =
                                let
                                    ( hand, drawingCard ) =
                                        case anim of
                                            Just (Draw PlayerA) ->
                                                ( List.take (List.length finalHand - 1) finalHand
                                                , List.head <| List.reverse finalHand
                                                )

                                            otherwise ->
                                                ( finalHand, Nothing )

                                    n =
                                        List.length hand

                                    finalN =
                                        List.length finalHand

                                    progress =
                                        Ease.outQuint <| res.tick / maxTick

                                    ( width, height, spacing ) =
                                        ( 0.1 * radius, 0.1 * radius, 35.0 )

                                    origin : Int -> Vec3
                                    origin count =
                                        vec3
                                            ((toFloat w / 2) - (0.5 * (width + spacing) * (toFloat count)))
                                            (toFloat h - height)
                                            0

                                    entity : Int -> WebGL.Entity
                                    entity i =
                                        Primitives.quad Clock.Shaders.fragment <|
                                            locals sword
                                                (interp progress (getPos i n) (getPos i finalN))
                                                (makeScale3 width height 1)
                                                (makeRotate pi <| vec3 0 0 1)

                                    getPos : Int -> Int -> Vec3
                                    getPos i count =
                                        Math.Vector3.add (origin count) <|
                                            vec3 ((toFloat i) * (width + spacing)) 0 0

                                    mainView : List WebGL.Entity
                                    mainView =
                                        List.map entity (List.range 0 (n - 1))

                                    drawView : List WebGL.Entity
                                    drawView =
                                        case drawingCard of
                                            Nothing ->
                                                []

                                            Just _ ->
                                                [ Primitives.quad Clock.Shaders.fragment <|
                                                    locals sword
                                                        (interp
                                                            progress
                                                            (vec3 (toFloat w) (toFloat h) 0)
                                                            (getPos n (n + 1))
                                                        )
                                                        (makeScale3 width height 1)
                                                        (makeRotate pi <| vec3 0 0 1)
                                                ]

                                    interp : Float -> Vec3 -> Vec3 -> Vec3
                                    interp t start end =
                                        let
                                            diff =
                                                Math.Vector3.sub end start
                                        in
                                            Math.Vector3.add start (Math.Vector3.scale t diff)
                                in
                                    mainView ++ drawView

                            otherHandView : Int -> List WebGL.Entity
                            otherHandView n =
                                let
                                    ( width, height, spacing ) =
                                        ( 0.1 * radius, 0.1 * radius, 35.0 )

                                    origin =
                                        vec3
                                            ((toFloat w / 2) - (0.5 * (width + spacing) * (toFloat n)))
                                            height
                                            0

                                    entity : Int -> WebGL.Entity
                                    entity i =
                                        Primitives.quad Clock.Shaders.fragment <|
                                            locals sword
                                                (Math.Vector3.add origin <|
                                                    vec3 ((toFloat i) * (width + spacing)) 0 0
                                                )
                                                (makeScale3 width height 1)
                                                (makeRotate 0 <| vec3 0 0 1)
                                in
                                    List.map entity (List.range 0 (n - 1))
                        in
                            List.concat
                                [ [ Primitives.circle <|
                                        locals circle
                                            (vec3 (toFloat w / 2) (toFloat h / 2) z)
                                            (makeScale3 (0.8 * radius) (0.8 * radius) 1)
                                            (makeRotate 0 <| vec3 0 0 1)
                                  ]
                                , stackView
                                , [ Primitives.circle <|
                                        locals circle
                                            (vec3 (toFloat w / 2) (toFloat h / 2) z)
                                            (makeScale3 (0.5 * radius) (0.5 * radius) 1)
                                            (makeRotate 0 <| vec3 0 0 1)
                                  , Primitives.circle <|
                                        locals circle
                                            (vec3 (toFloat w / 2) ((toFloat h / 2) - (0.65 * radius)) z)
                                            (makeScale3 (0.15 * radius) (0.15 * radius) 1)
                                            (makeRotate 0 <| vec3 0 0 1)
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
                                  ]
                                , handView model.hand
                                , otherHandView model.otherHand
                                ]

                    Nothing ->
                        []
                )
            ]
