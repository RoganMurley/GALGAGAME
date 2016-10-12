module Chat exposing (Model, addChatMessage, dragAt, dragEnd, dragStart, getPosition, init, view)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Mouse exposing (Position)

import Messages exposing (..)
import Util exposing (px)


type alias Model =
  {
    input : String
  , messages : List String
  , pos : Position
  , drag : Maybe Drag
  }

type alias Drag =
  {
    start : Position
  , current : Position
  }


init : Model
init =
  {
    input = ""
  , messages = []
  , pos = Position 0 0
  , drag = Nothing
  }

addChatMessage : String -> Model -> Model
addChatMessage message model =
  { model | messages = message :: model.messages }

getPosition : Model -> Position
getPosition { pos, drag } =
  case drag of
    Nothing ->
      pos

    Just { start, current } ->
      Position
        (pos.x + current.x - start.x)
        (pos.y + current.y - start.y)

draggable : Attribute Msg
draggable =
  on "mousedown" (Json.map DragStart Mouse.position)


dragStart : Model -> Position -> Model
dragStart model pos =
  { model | drag = (Just (Drag pos pos)) }

dragAt : Model -> Position -> Model
dragAt model pos =
  { model | drag = (Maybe.map (\{ start } -> Drag start pos) model.drag) }

dragEnd : Model -> Model
dragEnd model =
  { model | pos = getPosition model, drag = Nothing }

-- VIEW.

view : Model -> Html Msg
view model =
  let
    realPos : Position
    realPos = getPosition model
  in
    div [ class "chat", draggable, style [("top", px realPos.y), ("left", px realPos.x)] ]
      [
        div [ class "chat-input" ]
          [
            input [ onInput Input, value model.input ] []
          , button [ onClick (Send ("chat:" ++ model.input)) ] [ text "Send" ]
          ]
      , viewMessages model
      ]


viewMessages : Model -> Html Msg
viewMessages { messages } =
  let
    viewMessage : String -> Html Msg
    viewMessage msg = div [ class "message" ] [ text msg ]
  in
    div [ class "messages" ] (List.map viewMessage messages)
