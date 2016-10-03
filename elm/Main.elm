import Cmd.Extra exposing (message)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
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
      , hand = []
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
          "spectate:" ++ s
        Connected ->
          "chat:" ++ s
  in
    case msg of
      Input input ->
        ({ model | chat = { chat | input = input } }, Cmd.none)

      Send ->
        ({ model | chat = { chat | input = "" } }, send (buildChat model.mode chat.input))

      Receive str ->
        receive model str

      DragStart xy ->
        ({ model | chat = { chat | drag = (Just (Drag xy xy)) } }, Cmd.none)

      DragAt xy ->
        ({ model | chat = { chat | drag = (Maybe.map (\{start} -> Drag start xy) chat.drag) } }, Cmd.none)

      DragEnd _ ->
        ({ model | chat = { chat | pos = (getPosition chat), drag = Nothing } }, Cmd.none)

      IncCount ->
        (model, send "inc:")

      NewChatMsg str ->
        ({ model | chat = addChatMessage str chat}, Cmd.none)

      Sync str ->
        syncHand model str


receive : Model -> String -> (Model, Cmd Msg)
receive model msg =
  if (startsWith "chat:" msg) then
    (model, message (NewChatMsg (dropLeft (length "chat:") msg)))
  else if (startsWith "sync:" msg) then
    (model, message (Sync (dropLeft (length "sync:") msg)))
  else if (startsWith "accept:" msg) then
    ({ model | mode = Connected }, Cmd.none)
  else
    (model, message (NewChatMsg ("An error occured in decoding message from server... " ++ msg)))


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
        , viewGameState model
      ]


viewGameState : Model -> Html Msg
viewGameState model =
  let
    mode : SendMode
    mode = model.mode
  in
    case mode of
      Connected ->
        div []
          [
              viewOtherHand model.otherHand
            , viewHand model.hand
          ]
      otherwise ->
        div [] []


viewHand : Hand -> Html Msg
viewHand hand =
  let
    viewCard : Card -> Html Msg
    viewCard card = div [ class "card my-card", onClick IncCount ] [ text card ]
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

decodeHand : String -> Result String Hand
decodeHand msg =
  let
    result : Result String Hand
    result = Json.decodeString decoder msg
    decoder : Json.Decoder (List String)
    decoder = Json.list Json.string
  in
    result

syncHand : Model -> String -> (Model, Cmd Msg)
syncHand model msg =
  let
    result : Result String Hand
    result = decodeHand msg
  in
    case result of
      Ok value ->
        ({ model | hand = value }, Cmd.none)
      Err err ->
        (model, message (NewChatMsg ("Sync hand error: " ++ err)))
