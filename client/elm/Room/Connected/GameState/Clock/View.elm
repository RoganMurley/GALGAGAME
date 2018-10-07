module Clock.View exposing (view)

import Animation.Types exposing (Anim(..))
import Clock.Card exposing (cardTexture, colour)
import Clock.Hand exposing (handView, otherHandView)
import Clock.Primitives as Primitives
import Clock.Shaders
import Clock.Stack
import Clock.Types exposing (ClockParams, Model)
import Clock.Uniforms exposing (uniforms)
import Clock.Wave
import Connected.Messages as Connected
import GameState.Messages as GameState
import Hand.State exposing (maxHandLength)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled, height, width, style)
import Html.Events exposing (onClick)
import Main.Messages as Main
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Maybe.Extra as Maybe
import Model.Types exposing (Life)
import Render exposing (Params)
import Resolvable.State exposing (activeAnim, activeModel)
import Room.Messages as Room
import Stack.Types exposing (StackCard)
import Texture.State as Texture
import Texture.Types as Texture
import Util exposing (px)
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Params -> Model -> Texture.Model -> Html Main.Msg
view { w, h } { res, focus, entities } textures =
    let
        mTextures =
            Maybe.map2 (,)
                (Texture.load textures "striker/dagger.svg")
                (Texture.load textures "noise")

        locals =
            uniforms ( w, h )

        radius =
            if h < w then
                0.8 * (toFloat h / 2)
            else
                1.2 * (toFloat w / 2)

        anim =
            activeAnim res

        model =
            activeModel res

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
                , class "webgl-canvas"
                ]
                (case mTextures of
                    Just ( dagger, noise ) ->
                        List.concat
                            [ Clock.Stack.view params entities.stack resInfo textures
                            , focusImageView params textures focus
                            , [ Primitives.circle <|
                                    locals dagger
                                        (vec3 (toFloat w / 2) (toFloat h / 2) z)
                                        (makeScale3 (0.8 * radius) (0.8 * radius) 1)
                                        (makeRotate 0 <| vec3 0 0 1)
                                        (vec3 1 1 1)
                              ]
                            , [ Primitives.circle <|
                                    locals dagger
                                        (vec3 (toFloat w / 2) (toFloat h / 2) z)
                                        (makeScale3 (0.52 * radius) (0.52 * radius) 1)
                                        (makeRotate 0 <| vec3 0 0 1)
                                        (vec3 1 1 1)
                              , Primitives.circle <|
                                    locals dagger
                                        (vec3 (toFloat w / 2) ((toFloat h / 2) - (0.615 * radius)) z)
                                        (makeScale3 (0.13 * radius) (0.13 * radius) 1)
                                        (makeRotate 0 <| vec3 0 0 1)
                                        (vec3 1 1 1)
                              ]
                            , handView params entities.hand resInfo noise textures
                            , otherHandView params entities.otherHand resInfo noise textures
                            , Clock.Wave.view params resInfo dagger
                            , lifeOrbView params textures model.life model.otherLife
                            ]

                    Nothing ->
                        []
                )
            , div [ class "text-focus" ]
                [ focusTextView params focus
                ]
            , div [ class "clock-life-container" ]
                [ div
                    [ class "clock-life"
                    , style
                        [ ( "right", 0.49 * radius |> px )
                        , ( "top", 0.62 * radius |> px )
                        , ( "font-size", 0.18 * radius |> px )
                        ]
                    ]
                    [ lifeTextView model.life ]
                , div
                    [ class "clock-life other"
                    , style
                        [ ( "left", 0.5 * radius |> px )
                        , ( "bottom", 0.62 * radius |> px )
                        , ( "font-size", 0.18 * radius |> px )
                        ]
                    ]
                    [ lifeTextView model.otherLife ]
                ]
            , div [ class "clock-go" ]
                [ turnView
                    anim
                    focus
                    (List.length model.hand == maxHandLength)
                    model.turn
                ]
            ]


focusImageView : ClockParams -> Texture.Model -> Maybe StackCard -> List WebGL.Entity
focusImageView { w, h, radius } textures focus =
    let
        mTexture =
            Maybe.join <| Maybe.map (cardTexture textures << .card) focus
    in
        case mTexture of
            Just texture ->
                let
                    background =
                        case Maybe.map .owner focus of
                            Just owner ->
                                [ Primitives.fullCircle <|
                                    uniforms
                                        ( floor w, floor h )
                                        texture
                                        (vec3 (w * 0.5) (h * 0.5) 0)
                                        (makeScale3 (0.52 * radius) (0.52 * radius) 1)
                                        (makeRotate pi <| vec3 0 0 1)
                                        (Math.Vector3.scale 0.5 <| colour owner)
                                ]

                            Nothing ->
                                []
                in
                    background
                        ++ [ Primitives.quad Clock.Shaders.fragment <|
                                uniforms
                                    ( floor w, floor h )
                                    texture
                                    (vec3 (w * 0.5) (h * 0.45) 0)
                                    (makeScale3 (0.2 * radius) (0.2 * radius) 1)
                                    (makeRotate pi <| vec3 0 0 1)
                                    (vec3 1 1 1)
                           ]

            Nothing ->
                []


lifeOrbView : ClockParams -> Texture.Model -> Life -> Life -> List WebGL.Entity
lifeOrbView { w, h, radius } textures life otherLife =
    let
        mTexture =
            Texture.load textures "striker/dagger.svg"

        lifePercentage =
            toFloat life / 50

        otherLifePercentage =
            toFloat otherLife / 50
    in
        case mTexture of
            Just texture ->
                [ Primitives.fullCircle <|
                    uniforms
                        ( floor w, floor h )
                        texture
                        (vec3 (w * 0.5 - 0.6 * radius) (h * 0.5 + 0.75 * radius) 0)
                        (makeScale3
                            (lifePercentage * 0.15 * radius)
                            (lifePercentage * 0.15 * radius)
                            1
                        )
                        (makeRotate pi <| vec3 0 0 1)
                        (colour PlayerA)
                , Primitives.circle <|
                    uniforms
                        ( floor w, floor h )
                        texture
                        (vec3 (w * 0.5 - 0.6 * radius) (h * 0.5 + 0.75 * radius) 0)
                        (makeScale3 (0.15 * radius) (0.15 * radius) 1)
                        (makeRotate pi <| vec3 0 0 1)
                        (vec3 1 1 1)
                , Primitives.fullCircle <|
                    uniforms
                        ( floor w, floor h )
                        texture
                        (vec3 (w * 0.5 + 0.6 * radius) (h * 0.5 - 0.75 * radius) 0)
                        (makeScale3
                            (otherLifePercentage * 0.15 * radius)
                            (otherLifePercentage * 0.15 * radius)
                            1
                        )
                        (makeRotate pi <| vec3 0 0 1)
                        (colour PlayerB)
                , Primitives.circle <|
                    uniforms
                        ( floor w, floor h )
                        texture
                        (vec3 (w * 0.5 + 0.6 * radius) (h * 0.5 - 0.75 * radius) 0)
                        (makeScale3 (0.15 * radius) (0.15 * radius) 1)
                        (makeRotate pi <| vec3 0 0 1)
                        (vec3 1 1 1)
                ]

            Nothing ->
                []


focusTextView : ClockParams -> Maybe StackCard -> Html a
focusTextView { radius } stackCard =
    case stackCard of
        Nothing ->
            text ""

        Just { card } ->
            div
                [ style
                    [ ( "width", 0.7 * radius |> px )
                    ]
                ]
                [ div [ class "title" ] [ text card.name ]
                , div [ class "desc" ] [ text card.desc ]
                ]


lifeTextView : Life -> Html a
lifeTextView life =
    text <| toString life


turnView : Maybe Anim -> Maybe StackCard -> Bool -> WhichPlayer -> Html Main.Msg
turnView anim focus handFull turn =
    case ( anim, focus ) of
        ( Just (Overdraw _ _), _ ) ->
            div [] []

        ( Nothing, Nothing ) ->
            case turn of
                PlayerA ->
                    button
                        [ class "clock-turn"
                        , disabled handFull
                        , onClick <|
                            Main.RoomMsg <|
                                Room.ConnectedMsg <|
                                    Connected.GameStateMsg <|
                                        GameState.PlayingOnly <|
                                            GameState.TurnOnly <|
                                                GameState.EndTurn
                        ]
                        [ text "Go" ]

                PlayerB ->
                    div [ class "turn-status" ] [ text "Opponent's turn" ]

        _ ->
            div [] []