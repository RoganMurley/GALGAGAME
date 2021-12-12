module Chat.View exposing (htmlView, notifyView)

import Buttons.Types exposing (Buttons(..))
import Chat.Decoders exposing (chatDragEventDecoder)
import Chat.Messages exposing (Msg(..))
import Chat.Types exposing (Model)
import Dict
import Ease
import Game.Types exposing (Context)
import Html exposing (Html, a, div, input, text)
import Html.Attributes exposing (autofocus, class, classList, href, id, style, target, value)
import Html.Events exposing (on, onClick, onInput)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Uniforms exposing (uniColourMag)
import Util exposing (px)
import WebGL


htmlView : Model -> Html Msg
htmlView model =
    if model.visible then
        div
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
                        [ div [ class "chatbox__empty" ]
                            [ a
                                [ href "https://discord.gg/SVXXej4", target "_blank" ]
                                [ text "Join the GALGA community on Discord" ]
                            ]
                        ]

                    _ ->
                        List.map (\message -> div [] [ text message ]) model.messages
            , input [ class "chatbox__input", id "chat-input", autofocus True, onInput SetInput, value model.input ] []
            , div [ class "chatbox__close", onClick ToggleVisibility ]
                [ text "x"
                ]
            ]

    else
        text ""


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
