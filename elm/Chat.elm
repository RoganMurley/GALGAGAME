module Chat exposing (Model, addChatMessage, init, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Mouse exposing (Position)
import Drag exposing (Drag, draggable)
import Messages exposing (..)


type alias Model =
    { input : String
    , messages : List String
    , pos : Position
    , drag : Maybe Drag
    }


init : Model
init =
    { input = ""
    , messages = []
    , pos = Position 0 0
    , drag = Nothing
    }


addChatMessage : String -> Model -> Model
addChatMessage message model =
    { model | messages = message :: model.messages }



-- VIEW.


view : Model -> Html Msg
view model =
    div ([ class "chat" ] ++ (Drag.draggable model))
        [ div [ class "chat-input" ]
            [ input [ onInput Input, value model.input ] []
            , button [ onClick (Send ("chat:" ++ model.input)) ] [ text "Send" ]
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
        div [ class "messages" ] (List.map viewMessage messages)
