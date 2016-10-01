import Cmd.Extra exposing (message)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import String exposing (dropLeft, length, startsWith)
import WebSocket

import Messages exposing (Msg(..))


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
  , hand : Hand
  , otherHand : Hand
  , stack : PlayStack
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

type alias Hand =
  List Card

type alias PlayStack =
  List Card

type alias Card =
  String

type SendMode
  = Connecting
  | Connected

init : (Model, Cmd Msg)
init =
  let
    model : Model
    model = {
        chat = {
            input = ""
          , messages = []
          , pos = Position 0 0
          , drag = Nothing
        }
      , mode = Connecting
      , hand = [ "start" ]
      , otherHand = [ "start" ]
      , stack = [ "start" ]
    }
  in
    (model, Cmd.none)


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    chat : ChatModel
    chat = model.chat
    buildChat : SendMode -> String -> String
    buildChat m s =
      case m of
        Connecting ->
          "join:" ++ s
        Connected ->
          "chat:" ++ s
  in
    case msg of
      Input input ->
        ({ model | chat = { chat | input = input } }, Cmd.none)

      Send ->
        ({ model | chat = { chat | input = "" } }, send (buildChat model.mode chat.input))

      Receive str ->
        (model, receive str)

      DragStart xy ->
        ({ model | chat = { chat | drag = (Just (Drag xy xy)) } }, Cmd.none)

      DragAt xy ->
        ({ model | chat = { chat | drag = (Maybe.map (\{start} -> Drag start xy) chat.drag) } }, Cmd.none)

      DragEnd _ ->
        ({ model | chat = { chat | pos = (getPosition chat), drag = Nothing } }, Cmd.none)

      IncCount ->
        (model, send "inc:")

      NewChatMsg str ->
        ({ model | chat = addChatMessage str chat, mode = Connected }, Cmd.none)


receive : String -> Cmd Msg
receive msg =
  if (startsWith "chat:" msg) then
    message (NewChatMsg (dropLeft (length "chat:") msg))
  else
    message (NewChatMsg ("An error occured in decoding message from server... " ++ msg))


send : String -> Cmd Msg
send = WebSocket.send "ws://localhost:9160"


addChatMessage : String -> ChatModel -> ChatModel
addChatMessage message model =
  { model | messages = message :: model.messages }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
      WebSocket.listen "ws://localhost:9160" Receive
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
            , viewMessages model.chat
          ]
        , viewOtherHand model.otherHand
        , viewHand model.hand
      ]


viewHand : Hand -> Html Msg
viewHand hand =
  let
    viewCard : Card -> Html Msg
    viewCard card = div [ class "card my-card", onClick IncCount ] []
  in
    div [ class "hand my-hand" ] (List.map viewCard hand)


viewOtherHand : Hand -> Html Msg
viewOtherHand hand =
  let
    viewCard : Card -> Html Msg
    viewCard card = div [ class "card other-card" ] []
  in
    div [ class "hand other-hand" ] (List.map viewCard hand)


viewMessages : ChatModel -> Html Msg
viewMessages model =
  let
    viewMessage : String -> Html Msg
    viewMessage msg = div [ class "message" ] [ text msg ]
  in
    div [ class "messages" ] (List.map viewMessage model.messages)


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
