import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
  { input : String
  , messages : List String
  , mode : SendMode
  }

type SendMode
  = Connecting
  | Connected

init : (Model, Cmd Msg)
init =
  (Model "" [] Connecting, Cmd.none)


-- UPDATE

type Msg
  = Input String
  | Send
  | NewMessage String


update : Msg -> Model -> (Model, Cmd Msg)
update msg {input, messages, mode} =
  case msg of
    Input newInput ->
      (Model newInput messages mode, Cmd.none)

    Send ->
      case mode of
        Connecting ->
          (Model "" messages mode, WebSocket.send "ws://localhost:9160" ("Hi! I am " ++ input))
        Connected ->
          (Model "" messages mode, WebSocket.send "ws://localhost:9160" input)

    NewMessage str ->
      case str of
        otherwise ->
          (Model input (str :: messages) Connecting, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen "ws://localhost:9160" NewMessage


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] (List.map viewMessage model.messages)
    , input [onInput Input, value model.input] []
    , button [onClick Send] [text "Send"]
    ]


viewMessage : String -> Html msg
viewMessage msg =
  div [] [ text msg ]
