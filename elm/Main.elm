import Char exposing (isLower)
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
  , error : String
  , valid : Bool
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
      room = Connecting { name = "", error = "", valid = False }
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
    case room of

      Connecting { name } ->
        case msg of

          Input input ->
            ({ model | room = Connecting { name = input, error = snd (validateName name), valid = fst (validateName name) } }, Cmd.none)

          Send str ->
            (model, send model str)

          Receive str ->
            receive model str

          DragAt pos ->
            (model, Cmd.none)

          DragEnd pos ->
            (model, Cmd.none)

          NewChatMsg str ->
            ({ model | room = Connecting { name = "", error = str, valid = True } }, Cmd.none)

          otherwise ->
            Debug.crash "Unexpected action while not connected ;_;"

      Connected { chat, game } ->
        case msg of

          Input input ->
            ({ model | room = Connected { chat = { chat | input = input }, game = game }}, Cmd.none)

          Send str ->
            ({ model | room = Connected { chat = { chat | input = "" }, game = game } }, send model str)

          Receive str ->
            receive model str

          DragStart pos ->
            ({ model | room = Connected { chat = dragStart chat pos, game = game } }, Cmd.none)

          DragAt pos ->
            ({ model | room = Connected { chat = dragAt chat pos, game = game } }, Cmd.none)

          DragEnd pos ->
            ({ model | room = Connected { chat = dragEnd chat, game = game } }, Cmd.none)

          DrawCard ->
            (model, send model "draw:")

          NewChatMsg str ->
            ({ model | room = Connected { chat = addChatMessage str chat, game = game } }, Cmd.none)

          GameStateMsg gameMsg ->
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


-- VALIDATION
validateName : String -> (Bool, String)
validateName name =
  if length name > 20 then
    (False, "Userame too long!")
  else if String.isEmpty name then
    (False, "")
  else
    (True, "")


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
    Connecting { name, error, valid } ->
      div [ class "connecting-box" ]
        [
          div []
          [
            input [ onInput Input, placeholder "Username" ] []
          , button [ onClick (Send ("play:" ++ name)), disabled (not valid) ] [ text "Play" ]
          , button [ onClick (Send ("spectate:" ++ name)), disabled (not valid) ] [ text "Spec" ]
          ]
        , div [ class "error" ] [ text error ]
        ]
