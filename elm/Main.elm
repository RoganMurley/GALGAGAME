import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import WebSocket


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  {
    chat : ChatModel
  , mode : SendMode
  , hand : List String
  }

type alias ChatModel =
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

type SendMode
  = Connecting
  | Connected

init : (Model, Cmd Msg)
init =
  (Model (ChatModel "" [] (Position 0 0) Nothing) Connecting [ "start" ], Cmd.none)


-- UPDATE

type Msg
  = Input String
  | Send
  | NewMessage String
  | DragStart Position
  | DragAt Position
  | DragEnd Position


update : Msg -> Model -> (Model, Cmd Msg)
update msg {chat, mode, hand} =
  case msg of
    Input newInput ->
      (Model { chat | input = newInput } mode hand, Cmd.none)

    Send ->
      case mode of
        Connecting ->
          (Model { chat | input = "" } mode hand, WebSocket.send "ws://localhost:9160" ("Hi! I am " ++ chat.input))
        Connected ->
          (Model { chat | input = "" } mode hand, WebSocket.send "ws://localhost:9160" chat.input)

    NewMessage str ->
      (Model (addChatMessage str chat) Connected hand, Cmd.none)

    DragStart xy ->
      (Model { chat | drag = (Just (Drag xy xy)) } mode hand, Cmd.none)

    DragAt xy ->
      (Model { chat | drag = (Maybe.map (\{start} -> Drag start xy) chat.drag) } mode hand, Cmd.none)

    DragEnd _ ->
      (Model { chat | pos = (getPosition chat), drag = Nothing } mode hand, Cmd.none)


addChatMessage : String -> ChatModel -> ChatModel
addChatMessage message model =
  { model | messages = (message :: model.messages) }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
      WebSocket.listen "ws://localhost:9160" NewMessage
    , Mouse.moves DragAt
    , Mouse.ups DragEnd
  ]


-- VIEW

view : Model -> Html Msg
view model =
  let
    realPos = getPosition model.chat
  in
    div []
      [
        div [ class "chat", draggable, style [("top", px realPos.y), ("left", px realPos.x)] ]
          [
            div [ class "chat-input" ]
              [
                  input [ onInput Input, value model.chat.input ] []
                , button [ onClick Send ] [text "Send"]
              ]
            , div [ class "messages" ] (List.map viewMessage model.chat.messages)
          ]
        , div [ class "hand" ] (List.map viewCard model.hand)
      ]


viewCard : String -> Html Msg
viewCard card =
  div [ class "card" ] []


viewMessage : String -> Html msg
viewMessage msg =
  div [ class "message" ] [ text msg ]


px : Int -> String
px number =
  toString number ++ "px"


getPosition : ChatModel -> Position
getPosition {pos, drag} =
  case drag of
    Nothing ->
      pos

    Just {start,current} ->
      Position
        (pos.x + current.x - start.x)
        (pos.y + current.y - start.y)


draggable : Attribute Msg
draggable =
  on "mousedown" (Json.map DragStart Mouse.position)
