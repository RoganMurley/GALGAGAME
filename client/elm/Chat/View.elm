module Chat.View exposing (htmlView, notifyView)

import Assets.Types as Assets
import Buttons.Types exposing (Buttons(..))
import Chat.Decoders exposing (chatDragEventDecoder)
import Chat.Messages exposing (Msg(..))
import Chat.Types exposing (Model)
import Dict
import Ease
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import Html exposing (Html, a, div, input, span, text)
import Html.Attributes exposing (class, classList, href, id, style, target, value)
import Html.Events exposing (on, onClick, onInput, onMouseDown)
import Main.Types exposing (Flags)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Maybe
import Mouse exposing (MouseState(..))
import Random
import Random.List
import Render.Primitives
import Render.Uniforms exposing (uniColourMag)
import Util exposing (px)
import WebGL


htmlView : Flags -> Assets.Model -> Model -> Html Msg
htmlView flags assets model =
    let
        promos : List (Html Msg)
        promos =
            [ a
                [ href "https://discord.gg/SVXXej4", target "_blank" ]
                [ text "Join the GALGA community on Discord" ]
            , a
                [ href "/feedback", target "_blank" ]
                [ text "Let me know what you love/hate about GALGA" ]
            , a
                [ href "/league", target "_blank" ]
                [ text "Join the GALGA league" ]
            ]

        promo =
            Maybe.withDefault (text "") <|
                Tuple.first <|
                    Tuple.first <|
                        Random.step
                            (Random.List.choose promos)
                            (Random.initialSeed flags.seed)

        { radius, w, h } =
            bareContextInit flags.dimensions assets NoMouse

        width =
            0.12 * radius * 2

        height =
            0.12 * radius * 2

        left =
            w * 0.5 - 0.5 * radius - width * 0.5

        top =
            0.8 * h - height * 0.5
    in
    div []
        [ div
            [ style "visibility" <|
                if model.visible then
                    "visible"

                else
                    "hidden"
            ]
            [ div
                [ class "chatbox-close-mobile"
                , onMouseDown ToggleVisibility
                ]
                []
            , div
                [ classList
                    [ ( "chatbox", True )
                    , ( "chatbox--drag", model.drag /= Nothing )
                    ]
                , style "top" (toFloat model.pos.y |> px)
                , style "left" (toFloat model.pos.x |> px)
                , on "mousedown" chatDragEventDecoder
                ]
                [ div
                    [ class "chatbox__messages" ]
                  <|
                    case model.messages of
                        [] ->
                            [ div [ class "chatbox__empty" ] [ promo ]
                            ]

                        _ ->
                            List.map
                                (\{ username, message } ->
                                    div []
                                        [ span [ class "chatbox__username" ] [ text <| username ++ ": " ]
                                        , span [ class "chatbox__message" ] [ text message ]
                                        ]
                                )
                                model.messages
                , input [ class "chatbox__input", id "chat-input", onInput SetInput, value model.input ] []
                , div [ class "chatbox__close", onClick ToggleVisibility ]
                    [ text "x"
                    ]
                ]
            ]
        , div
            [ id "ios-chat-button-hack"
            , style "top" (top |> px)
            , style "left" (left |> px)
            , style "width" (width |> px)
            , style "height" (height |> px)
            ]
            []
        ]


notifyView : Model -> Buttons -> Context -> List WebGL.Entity
notifyView model (Buttons buttons) ctx =
    if model.notify then
        case Dict.get "toggleChat" buttons of
            Just { x, y, width, hover } ->
                let
                    scalingFactor =
                        0.3

                    hoverPop =
                        scalingFactor * 10 * Ease.outQuint (hover / 300)

                    offset =
                        0.075 * ctx.radius + hoverPop
                in
                [ Render.Primitives.fullCircle <|
                    uniColourMag ctx
                        (vec3 1 0 0)
                        1.0
                        { scale = scalingFactor * width + hoverPop
                        , position = Math.Vector2.add (vec2 x y) (vec2 offset -offset)
                        , rotation = 0
                        }
                ]

            Nothing ->
                []

    else
        []
