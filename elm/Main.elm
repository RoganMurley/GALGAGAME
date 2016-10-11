import Cmd.Extra exposing (message)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Mouse
import String exposing (dropLeft, length, startsWith)
import WebSocket

import Chat exposing (addChatMessage, dragAt, dragEnd, dragStart, getPosition)
import GameState exposing (Card, Hand, Model, view)
import Messages exposing (GameMsg(..), Msg(..))


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
    room : RoomModel
  , hostname : String
  }

type RoomModel =
    Connecting
  {
    name : String
  }
  | Connected
  {
    chat : Chat.Model
  , game : GameState.Model
  }

type alias Flags =
  {
    hostname : String
  }

init : Flags -> (Model, Cmd Msg)
init { hostname } =
  let
    model : Model
    model =
    {
      room = Connecting { name = "" }
    , hostname = hostname
    }
  in
    (model, Cmd.none)


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    room : RoomModel
    room = model.room
  in
    case msg of
      Input input ->
        case room of
          Connecting { name } ->
            ({ model | room = Connecting { name = input } }, Cmd.none)
          Connected { chat, game } ->
            ({ model | room = Connected { chat = { chat | input = input }, game = game }}, Cmd.none)

      Send ->
        case room of
          Connecting { name } ->
            (model, send model ("spectate:" ++ name))
          Connected { chat, game } ->
            ({ model | room = Connected { chat = { chat | input = "" }, game = game } }, send model ("chat:" ++ chat.input))

      Receive str ->
        receive model str

      DragStart pos ->
        case room of
          Connecting _ ->
            Debug.crash "Dragging chatbox while not connected :/ WTF"
          Connected { chat, game } ->
            ({ model | room = Connected { chat = dragStart chat pos, game = game } }, Cmd.none)

      DragAt pos ->
        case room of
          Connecting m ->
            ({ model | room = Connecting m }, Cmd.none)
          Connected { chat, game } ->
            ({ model | room = Connected { chat = dragAt chat pos, game = game } }, Cmd.none)

      DragEnd _ ->
        case room of
          Connecting m ->
            ({ model | room = Connecting m }, Cmd.none)
          Connected { chat, game } ->
            ({ model | room = Connected { chat = dragEnd chat, game = game } }, Cmd.none)

      DrawCard ->
        (model, send model "draw:")

      NewChatMsg str ->
        case room of
          Connecting _ ->
            Debug.crash "New chat message while not connected :/ WTF"
          Connected { chat, game } ->
            ({ model | room = Connected { chat = addChatMessage str chat, game = game } }, Cmd.none)

      GameStateMsg gameMsg ->
        case room of
          Connecting { name } ->
            Debug.crash "Updating gamestate while disconnected!"
          Connected { chat, game } ->
            ({ model | room = Connected { chat = chat, game = GameState.update gameMsg game } }, Cmd.none)


receive : Model -> String -> (Model, Cmd Msg)
receive model msg =
  if (startsWith "chat:" msg) then
    (model, message (NewChatMsg (dropLeft (length "chat:") msg)))
  else if (startsWith "sync:" msg) then
    (model, message (GameStateMsg (Sync (dropLeft (length "sync:") msg))))
  else if (startsWith "accept:" msg) then
    ({ model | room = Connected { chat = Chat.init, game = GameState.init } }, Cmd.none)
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
    Connected { chat, game } ->
      div []
        [
          Chat.view chat
        , GameState.view game
        ]
    otherwise ->
      div []
        [
          input [ onInput Input ] []
        , button [ onClick Send ] [ text "Spectate" ]
        ]
