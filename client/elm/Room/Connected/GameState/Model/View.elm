module Model.View exposing (view)

import Animation.Types exposing (Anim(..))
import Card.State exposing (cardTexture)
import Colour
import Connected.Messages as Connected
import Game.State exposing (contextInit)
import Game.Types exposing (Model, Context)
import GameState.Messages as GameState
import Hand.State exposing (maxHandLength)
import Hand.View as Hand
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled, height, width, style)
import Html.Events exposing (onClick)
import Main.Messages as Main
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Maybe.Extra as Maybe
import Model.Wave as Wave
import Render.Primitives
import Render.Shaders
import Render.Types exposing (Params)
import Render.Uniforms exposing (uni, uniColour)
import Room.Messages as Room
import Stack.Types exposing (StackCard)
import Stack.View as Stack
import Texture.Types as Texture
import Util exposing (px)
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Params -> Model -> Texture.Model -> Html Main.Msg
view { w, h } { res, focus, entities } textures =
    let
        ctx =
            contextInit ( w, h ) res textures
    in
        div [ class "clock" ]
            [ WebGL.toHtml
                [ width w
                , height h
                , class "webgl-canvas"
                ]
                (List.concat <|
                    List.map ((|>) ctx)
                        [ Stack.view entities.stack
                        , focusImageView focus
                        , circlesView
                        , Hand.view entities.hand
                        , Hand.otherView entities.otherHand
                        , lifeOrbView
                        , Wave.view
                        , Hand.millView
                        ]
                )
            , div [ class "text-focus" ] [ focusTextView ctx focus ]
            , div [ class "clock-life-container" ] (lifeTextView ctx)
            , div [ class "clock-go" ] [ turnView ctx focus ]
            ]


circlesView : Context -> List WebGL.Entity
circlesView ({ w, h, radius } as ctx) =
    let
        pos =
            vec2 (w / 2) (h / 2)
    in
        List.map (Render.Primitives.circle << uni ctx)
            [ { scale = 0.8 * radius, position = pos, rotation = 0 }
            , { scale = 0.52 * radius, position = pos, rotation = 0 }
            , { scale = 0.13 * radius, position = pos, rotation = 0 }
            ]


focusImageView : Maybe StackCard -> Context -> List WebGL.Entity
focusImageView focus ({ w, h, radius, textures } as ctx) =
    let
        background =
            case Maybe.map .owner focus of
                Just owner ->
                    [ Render.Primitives.fullCircle <|
                        uniColour ctx
                            (Colour.focusBackground owner)
                            { scale = 0.52 * radius
                            , position = vec2 (w * 0.5) (h * 0.5)
                            , rotation = 0
                            }
                    ]

                Nothing ->
                    []
    in
        case Maybe.join <| Maybe.map (cardTexture textures << .card) focus of
            Just texture ->
                background
                    ++ [ Render.Primitives.quad Render.Shaders.fragment
                            { rotation = makeRotate pi (vec3 0 0 1)
                            , scale = makeScale3 (0.2 * radius) (0.2 * radius) 1
                            , color = Colour.white
                            , pos = vec3 (w * 0.5) (h * 0.45) 0
                            , worldRot = makeRotate 0 (vec3 0 0 1)
                            , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                            , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                            , texture = texture
                            }
                       ]

            Nothing ->
                []


lifeOrbView : Context -> List WebGL.Entity
lifeOrbView ({ w, h, radius, model } as ctx) =
    let
        lifePercentage =
            toFloat model.life / 50

        otherLifePercentage =
            toFloat model.otherLife / 50
    in
        [ Render.Primitives.fullCircle <|
            uniColour ctx
                (Colour.card PlayerA)
                { scale = lifePercentage * 0.15 * radius
                , position = vec2 (w * 0.5 - 0.6 * radius) (h * 0.5 + 0.75 * radius)
                , rotation = 0
                }
        , Render.Primitives.circle <|
            uni ctx
                { scale = 0.15 * radius
                , position = vec2 (w * 0.5 - 0.6 * radius) (h * 0.5 + 0.75 * radius)
                , rotation = 0
                }
        , Render.Primitives.fullCircle <|
            uniColour ctx
                (Colour.card PlayerB)
                { scale = otherLifePercentage * 0.15 * radius
                , position = vec2 (w * 0.5 + 0.6 * radius) (h * 0.5 - 0.75 * radius)
                , rotation = 0
                }
        , Render.Primitives.circle <|
            uni ctx
                { scale = otherLifePercentage * 0.15 * radius
                , position = vec2 (w * 0.5 + 0.6 * radius) (h * 0.5 - 0.75 * radius)
                , rotation = 0
                }
        ]


focusTextView : Context -> Maybe StackCard -> Html a
focusTextView { radius } focus =
    case focus of
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


lifeTextView : Context -> List (Html a)
lifeTextView { radius, model } =
    [ div
        [ class "clock-life"
        , style
            [ ( "right", 0.49 * radius |> px )
            , ( "top", 0.62 * radius |> px )
            , ( "font-size", 0.18 * radius |> px )
            ]
        ]
        [ text <| toString model.life ]
    , div
        [ class "clock-life other"
        , style
            [ ( "left", 0.5 * radius |> px )
            , ( "bottom", 0.62 * radius |> px )
            , ( "font-size", 0.18 * radius |> px )
            ]
        ]
        [ text <| toString model.otherLife ]
    ]


turnView : Context -> Maybe StackCard -> Html Main.Msg
turnView { anim, model } focus =
    let
        handFull =
            List.length model.hand == maxHandLength
    in
        case ( anim, focus ) of
            ( Mill _ _, _ ) ->
                div [] []

            ( NullAnim, Nothing ) ->
                case model.turn of
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
