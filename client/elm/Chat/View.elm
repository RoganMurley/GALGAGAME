module Chat.View exposing (view)

import Chat.Messages exposing (Msg(..))
import Chat.Types exposing (Model)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)


view : Model -> Html Msg
view model =
    if model.visible then
        div [ class "chatbox" ]
            [ div [ class "chatbox__messages" ]
                (List.map (\message -> div [] [ text message ]) model.messages)
            , input [ class "chatbox__input", onInput SetInput, value model.input ] []
            , div [ class "chatbox__close", onClick ToggleVisibility ] [ text "x" ]
            ]

    else
        text ""
