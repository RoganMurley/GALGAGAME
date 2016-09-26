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
  , hand : List String
  }


init : (Model, Cmd Msg)
init =
  (Model "" [] [ "start" ], Cmd.none)


-- UPDATE

type Msg
  = Input String
  | Send
  | NewMessage String


update : Msg -> Model -> (Model, Cmd Msg)
update msg {input, messages, hand} =
  case msg of
    Input newInput ->
      (Model newInput messages hand, Cmd.none)

    Send ->
      (Model "" messages hand, WebSocket.send "ws://localhost:9160" ("Hi! I am " ++ input))

    NewMessage str ->
      (Model input (str :: messages) hand, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen "ws://localhost:9160" NewMessage


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [
      div [ class "chat" ]
        [
          div [ class "chat-input" ]
            [
              input [ onInput Input ] []
              , button [ onClick Send ] [text "Send"]
            ]
          , div [ class "messages" ] (List.map viewMessage model.messages)
        ]
      , div [ class "hand" ] (List.map viewCard model.hand)
    ]

viewCard : String -> Html Msg
viewCard card =
  div [ class "card" ] []

viewMessage : String -> Html msg
viewMessage msg =
  div [] [ text msg ]
