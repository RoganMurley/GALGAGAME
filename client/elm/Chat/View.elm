module Chat.View exposing (htmlView, notifyView)

import Buttons.Types exposing (Buttons(..))
import Chat.Messages exposing (Msg(..))
import Chat.Types exposing (Model)
import Dict
import Ease
import Game.Types exposing (Context)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (autofocus, class, value)
import Html.Events exposing (onClick, onInput)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Uniforms exposing (uniColourMag)
import WebGL


htmlView : Model -> Html Msg
htmlView model =
    if model.visible then
        div
            [ class "chatbox" ]
            [ div
                [ class "chatbox__messages" ]
              <|
                case model.messages of
                    [] ->
                        [ div [ class "chatbox__empty" ] [ text "It's quiet in here..." ] ]

                    _ ->
                        List.map (\message -> div [] [ text message ]) model.messages
            , input [ class "chatbox__input", autofocus True, onInput SetInput, value model.input ] []
            , div [ class "chatbox__close", onClick ToggleVisibility ] [ text "x" ]
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
