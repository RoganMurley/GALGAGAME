import Cmd.Extra exposing (message)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))
import Mouse
import String exposing (dropLeft, length, startsWith)
import WebSocket

import Chat exposing (addChatMessage, dragAt, dragEnd, dragStart, getPosition)
import GameState exposing (Card, Hand, Model, view)
import Messages exposing (Msg(..))


main =
  App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  {
    chat : Chat.Model
  , room : RoomModel
  , hostname : String
  }

type RoomModel =
    Connecting
  | Connected
  {
    game : GameState.Model
  }

type alias Flags =
  {
    hostname : String
  }

init : Flags -> (Model, Cmd Msg)
init { hostname } =
  let
    model : Model
    model = {
      chat = Chat.init
    , room = Connecting
    , hostname = hostname
    }
  in
    (model, Cmd.none)


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    chat : Chat.Model
    chat = model.chat
    buildChat : RoomModel -> String -> String
    buildChat m s =
      case m of
        Connecting ->
          "spectate:" ++ s
        Connected _ ->
          "chat:" ++ s
  in
    case msg of
      Input input ->
        ({ model | chat = { chat | input = input } }, Cmd.none)

      Send ->
        ({ model | chat = { chat | input = "" } }, send model (buildChat model.room chat.input))

      Receive str ->
        receive model str

      DragStart pos ->
        ({ model | chat = dragStart model.chat pos }, Cmd.none)

      DragAt pos ->
        ({ model | chat = dragAt model.chat pos }, Cmd.none)

      DragEnd _ ->
        ({ model | chat = dragEnd model.chat }, Cmd.none)

      DrawCard ->
        (model, send model "draw:")

      NewChatMsg str ->
        ({ model | chat = addChatMessage str chat}, Cmd.none)

      Sync str ->
        syncHands model str


receive : Model -> String -> (Model, Cmd Msg)
receive model msg =
  if (startsWith "chat:" msg) then
    (model, message (NewChatMsg (dropLeft (length "chat:") msg)))
  else if (startsWith "sync:" msg) then
    (model, message (Sync (dropLeft (length "sync:") msg)))
  else if (startsWith "accept:" msg) then
    ({ model | room = Connected { game = GameState.init } }, Cmd.none)
  else
    (model, message (NewChatMsg ("An error occured in decoding message from server... " ++ msg)))


send : Model -> String -> Cmd Msg
send model = WebSocket.send ("ws://" ++ model.hostname ++ ":9160")


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
    WebSocket.listen ("ws://" ++ model.hostname ++ ":9160") Receive
  , Mouse.moves DragAt
  , Mouse.ups DragEnd
  ]


-- VIEW

view : Model -> Html Msg
view model =
  case model.room of
    Connected { game } ->
      div []
        [
          Chat.view model.chat
        , GameState.view game
        ]
    otherwise ->
      div []
        [
          input [ onInput Input, value model.chat.input ] []
        , button [ onClick Send ] [ text "Spectate" ]
        ]

decodeHands : String -> Result String (Hand, Hand)
decodeHands msg =
  let
    result : Result String (Hand, Hand)
    result = Json.decodeString handDecoder msg
    handDecoder : Json.Decoder (Hand, Hand)
    handDecoder =
      Json.object2 (,)
        ("handPA" := Json.list cardDecoder)
        ("handPB" := Json.list cardDecoder)
    cardDecoder : Json.Decoder Card
    cardDecoder =
      Json.object4 Card
        ("name" := Json.string)
        ("desc" := Json.string)
        ("imageURL" := Json.string)
        ("cardColor" := Json.string)

  in
    result

syncHands : Model -> String -> (Model, Cmd Msg)
syncHands model msg =
  let
    room : RoomModel
    room = model.room
    result : Result String (Hand, Hand)
    result = decodeHands msg
  in
    case room of
      Connecting ->
        Debug.crash "Not connected yet received message, should never happen."
      Connected { game } ->
        case result of
          Ok (paHand, pbHand) ->
            ({ model | room = Connected { game = { game | hand = paHand, otherHand = pbHand } } }, Cmd.none)
          Err err ->
            (model, message (NewChatMsg ("Sync hand error: " ++ err)))
