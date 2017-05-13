module Chat.View exposing (view)

import Chat.Types exposing (Model)
import Drag.View as Drag
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Main.Messages exposing (Msg(Input, Send))


view : Model -> Html Msg
view model =
    div ([ class "chat" ] ++ (Drag.draggable model))
        [ div [ class "chat-input" ]
            [ input [ onInput Input, value model.input ] []
            , button [ onClick (Send ("chat:" ++ model.input)) ] [ text "send" ]
            ]
        , viewMessages model
        ]


viewMessages : Model -> Html Msg
viewMessages { messages } =
    let
        viewMessage : String -> Html Msg
        viewMessage msg =
            div [ class "message" ] [ text msg ]
    in
        div [ class "messages" ] <| List.map viewMessage messages
