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
  App.programWithFlags
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
  , game : GameModel
  , hostname : String
  }

type alias GameModel =
  {
    hand : Hand
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

type alias Flags =
  {
    hostname : String
  }

init : Flags -> (Model, Cmd Msg)
init { hostname } =
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
      , game = {
          hand = []
        , otherHand = [ "start" ]
        , stack = [ "start" ]
      }
      , hostname = hostname
    }
  in
    (model, Cmd.none)


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    game : GameModel
    game = model.game
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
        ({ model | chat = { chat | input = "" } }, send model (buildChat model.mode chat.input))

      Receive str ->
        receive model str

      DragStart xy ->
        ({ model | chat = { chat | drag = (Just (Drag xy xy)) } }, Cmd.none)

      DragAt xy ->
        ({ model | chat = { chat | drag = (Maybe.map (\{start} -> Drag start xy) chat.drag) } }, Cmd.none)

      DragEnd _ ->
        ({ model | chat = { chat | pos = (getPosition chat), drag = Nothing } }, Cmd.none)

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
    ({ model | mode = Connected }, Cmd.none)
  else
    (model, message (NewChatMsg ("An error occured in decoding message from server... " ++ msg)))


send : Model -> String -> Cmd Msg
send model = WebSocket.send ("ws://" ++ model.hostname ++ ":9160")


addChatMessage : String -> ChatModel -> ChatModel
addChatMessage message model =
  { model | messages = message :: model.messages }

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
    game : GameModel
    game = model.game
    mode : SendMode
    mode = model.mode
  in
    case mode of
      Connected ->
        div []
          [
              viewOtherHand game.otherHand
            , viewHand game.hand
          ]
      otherwise ->
        div [] []


viewHand : Hand -> Html Msg
viewHand hand =
  let
    viewCard : Card -> Html Msg
    viewCard card = div
      [
          class "card my-card"
        , onClick DrawCard
      ]
      [
          div [ class "card-title" ] [ text "Dagger" ]
        , div
          [
              class "card-picture"
            , style [ ("background-image", "url(\"img/" ++ card ++ "\")") ]
          ] []
        , div [ class "card-desc" ] [ text "Hurt for 100" ]
      ]
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

decodeHands : String -> Result String (Hand, Hand)
decodeHands msg =
  let
    result : Result String (Hand, Hand)
    result = Json.decodeString decoder msg
    decoder : Json.Decoder (List String, List String)
    decoder =
      Json.object2 (,)
        ("paHand" := Json.list Json.string)
        ("pbHand" := Json.list Json.string)
  in
    result

syncHands : Model -> String -> (Model, Cmd Msg)
syncHands model msg =
  let
    game : GameModel
    game = model.game
    result : Result String (Hand, Hand)
    result = decodeHands msg
  in
    case result of
      Ok (paHand, pbHand) ->
        ({ model | game = { game | hand = paHand, otherHand = pbHand } }, Cmd.none)
      Err err ->
        (model, message (NewChatMsg ("Sync hand error: " ++ err)))
