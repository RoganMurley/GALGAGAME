module Chat.View exposing (view)

import Chat.Messages exposing (Msg(..))
import Chat.Types exposing (Model)
import Drag.View as Drag
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Main.Messages as Main exposing (Msg(ChatMsg))


view : Model -> Html Main.Msg
view model =
    div ([ class "chat" ] ++ (Drag.draggable model))
        [ div [ class "chat-input" ]
            [ input
                [ onInput <| ChatMsg << Input, value model.input ]
                []
            , button
                [ onClick <| ChatMsg <| Send ]
                [ text "send" ]
            ]
        , viewMessages model.messages
        ]


viewMessages : List String -> Html msg
viewMessages messages =
    let
        viewMessage : String -> Html msg
        viewMessage msg =
            div
                [ class "message" ]
                [ text msg ]
    in
        div
            [ class "messages" ]
            (List.map viewMessage messages)
