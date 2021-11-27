module Chat.View exposing (view)

import Chat.Messages exposing (Msg(..))
import Chat.Types exposing (Model)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


view : Model -> Html Msg
view { messages, visible } =
    if visible then
        div [ class "chatbox" ]
            [ div [ class "chatbox__messages" ]
                (List.map (\message -> div [] [ text message ]) messages)
            , input [ class "chatbox__input" ] []
            , div [ class "chatbox__close", onClick (SetVisibility False) ] [ text "x" ]
            ]

    else
        text ""
