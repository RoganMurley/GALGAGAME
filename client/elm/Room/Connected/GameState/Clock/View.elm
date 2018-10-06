module Clock.View exposing (view)

import Animation.State exposing (animToResTickMax)
import Animation.Types exposing (Anim(..))
import Card.Types exposing (Card)
import Clock.Card exposing (cardTexture)
import Clock.Hand exposing (handView, otherHandView)
import Clock.Primitives as Primitives
import Clock.Shaders
import Clock.Stack
import Clock.Types exposing (ClockParams, Model)
import Clock.Uniforms exposing (uniforms)
import Clock.Wave
import Connected.Messages as Connected
import Ease
import GameState.Messages as GameState
import Hand.State exposing (maxHandLength)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Main.Messages as Main
import Math.Matrix4 exposing (makeRotate, makeScale3, makeLookAt, makeOrtho)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import Model.Types exposing (Life)
import Raymarch.Types exposing (Params(..))
import Resolvable.State exposing (activeAnim, activeModel)
import Room.Messages as Room
import Texture.State as Texture
import Texture.Types as Texture
import Util exposing (px)
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Params -> Model -> Texture.Model -> Html Main.Msg
view (Params _ ( w, h )) { res, focus, mouse, entities } textures =
    let
        mTextures =
            Maybe.map2 (,)
                (Texture.load textures "striker/dagger.svg")
                (Texture.load textures "noise")

        locals =
            uniforms 0 ( w, h )

        radius =
            if h < w then
                0.8 * (toFloat h / 2)
            else
                1.2 * (toFloat w / 2)

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
                    Just ( dagger, noise ) ->
                        List.concat
                            [ Clock.Stack.view params entities.stack resInfo textures
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
                            , handView params model.hand entities.hand resInfo noise textures
                            , otherHandView params model.otherHand entities.otherHand resInfo noise textures
                            , Clock.Wave.view params resInfo dagger
                            , focusImageView params textures focus
                            , lifeOrbView params textures model.life model.otherLife
                            ]

                    Nothing ->
                        []
                )
            , div [ class "text-focus" ]
                [ textView params focus
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


focusImageView : ClockParams -> Texture.Model -> Maybe Card -> List WebGL.Entity
focusImageView { w, h, radius } textures focus =
    let
        mTexture =
            Maybe.join <| Maybe.map (cardTexture textures) focus
    in
        case mTexture of
            Just texture ->
                [ Primitives.quad Clock.Shaders.fragment <|
                    uniforms 0
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
    in
        case mTexture of
            Just texture ->
                [ Primitives.circle <|
                    uniforms 0
                        ( floor w, floor h )
                        texture
                        (vec3 (w * 0.5 - 0.6 * radius) (h * 0.5 + 0.75 * radius) 0)
                        (makeScale3 (0.15 * radius) (0.15 * radius) 1)
                        (makeRotate pi <| vec3 0 0 1)
                        (vec3 1 1 1)
                , Primitives.circle <|
                    uniforms 0
                        ( floor w, floor h )
                        texture
                        (vec3 (w * 0.5 + 0.6 * radius) (h * 0.5 - 0.75 * radius) 0)
                        (makeScale3 (0.15 * radius) (0.15 * radius) 1)
                        (makeRotate pi <| vec3 0 0 1)
                        (vec3 1 1 1)
                ]

            Nothing ->
                []


textView : ClockParams -> Maybe Card -> Html a
textView { radius } card =
    case card of
        Nothing ->
            text ""

        Just { name, desc } ->
            div
                [ style
                    [ ( "width", 0.7 * radius |> px )
                    ]
                ]
                [ div [ class "title" ] [ text name ]
                , div [ class "desc" ] [ text desc ]
                ]


lifeTextView : Life -> Html a
lifeTextView life =
    text <| toString life


turnView : Maybe Anim -> Maybe Card -> Bool -> WhichPlayer -> Html Main.Msg
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
