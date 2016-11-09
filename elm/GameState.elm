module GameState exposing (Card, GameState(..), Hand, Model, Turn, WhichPlayer(..), init, stateUpdate, stateView, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))

import Messages exposing (GameMsg(Sync), Msg(DrawCard, EndTurn, PlayCard))


-- TYPES.

type GameState =
    Waiting
  | PlayingGame Model
  | Victory WhichPlayer
  | Draw

type alias Model =
  {
    hand : Hand
  , otherHand : Hand
  , stack : Stack
  , turn : Turn
  , life : Life
  , otherLife : Life
  }

type alias Hand =
  List Card

type alias Stack =
  List StackCard

type alias Card =
  {
    name : String
  , desc : String
  , imgURL: String
  }

type WhichPlayer
  = PlayerA
  | PlayerB

type alias Turn =
  WhichPlayer

type alias StackCard =
  {
    owner : WhichPlayer
  , card : Card
  }

type alias Life =
  Int


-- INITIAL MODEL.

init : Model
init =
  {
    hand = []
  , otherHand = []
  , stack = []
  , turn = PlayerA
  , life = 1000
  , otherLife = 1000
  }


-- VIEWS.
stateView : GameState -> Html Msg
stateView state =
  case state of
    Waiting ->
      div [ class "waiting" ] [ text "Waiting for opponent..." ]
    PlayingGame model ->
      view model
    Victory PlayerA ->
      div [ class "victory" ] [ text "VICTORY" ]
    Victory PlayerB ->
      div [ class "defeat" ] [ text "DEFEAT" ]
    Draw ->
      div [ class "draw" ] [ text "DRAW" ]

view : Model -> Html Msg
view model =
  div []
    [
      viewOtherHand model.otherHand
    , viewHand model.hand
    , viewStack model.stack
    , viewTurn model.turn
    , viewLife (model.life, model.otherLife)
    ]

viewHand : Hand -> Html Msg
viewHand hand =
  let
    viewCard : Card -> Html Msg
    viewCard { name, desc, imgURL } = div
      [
        class "card my-card"
      , onClick (PlayCard name)
      ]
      [
          div [ class "card-title" ] [ text name ]
        , div
          [
            class "card-picture"
          , style [ ("background-image", "url(\"img/" ++ imgURL ++ "\")") ]
          ] []
        , div [ class "card-desc" ] [ text desc ]
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

viewTurn : Turn -> Html Msg
viewTurn turn =
  case turn of
    PlayerA ->
      button [ class "turn-indi pass-button", onClick EndTurn ] [ text "Pass" ]
    PlayerB ->
      div [ class "turn-indi enemy-turn" ] [ text "Opponent's Turn" ]

viewLife : (Life, Life) -> Html Msg
viewLife (myLife, otherLife) =
  div
    [ class "life" ]
    [
      div [ class "life-counter" ] [ text ("Opponent HP: " ++ (toString otherLife)) ]
    , div [ class "life-counter" ] [ text ("Your HP: " ++ (toString myLife)) ]
    ]

viewStack: Stack -> Html Msg
viewStack stack =
  let
    viewCard : Card -> Html Msg
    viewCard { name, desc, imgURL } = div
      [
        class "card"
      ]
      [
          div [ class "card-title" ] [ text name ]
        , div
          [
            class "card-picture"
          , style [ ("background-image", "url(\"img/" ++ imgURL ++ "\")") ]
          ] []
        , div [ class "card-desc" ] [ text desc ]
      ]
    viewStackCard : StackCard -> Html Msg
    viewStackCard { owner, card } =
      case owner of
        PlayerA ->
          div [ class "playera" ] [ viewCard card ]
        PlayerB ->
          div [ class "playerb" ] [ viewCard card ]
  in
    div
      [ class "stack-container" ]
      [
        div [ class "stack" ] (List.map viewStackCard stack)
      ]

-- UPDATE
stateUpdate : GameMsg -> GameState -> GameState
stateUpdate msg state =
  case msg of
    Sync str ->
      syncState state str

syncState : GameState -> String -> GameState
syncState model msg =
  decodeState msg

decodeState : String -> GameState
decodeState msg =
  case decodePlaying msg of
    Ok playingState ->
      playingState
    Err err1 ->
      case decodeWaiting msg of
        Ok waitingState ->
          waitingState
        Err err2 ->
          case decodeVictory msg of
            Ok victoryState ->
              victoryState
            Err err3 ->
              case decodeDraw msg of
                Ok drawState ->
                  drawState
                Err err4 ->
                  Debug.crash
                    (
                         "Error 1:\n" ++ err1
                    ++ "\nError 2:\n" ++ err2
                    ++ "\nError 3:\n" ++ err3
                    ++ "\nError 4:\n" ++ err4
                    )

decodeWaiting : String -> Result String GameState
decodeWaiting msg =
  let
    decoder : Json.Decoder GameState
    decoder = Json.object1 (\_ -> Waiting) ("waiting" := Json.bool)
  in
    Json.decodeString decoder msg

decodeVictory : String -> Result String GameState
decodeVictory msg =
  let
    decoder : Json.Decoder GameState
    decoder = Json.object1 Victory ("victory" := whichDecoder)
  in
    Json.decodeString decoder msg

decodeDraw : String -> Result String GameState
decodeDraw msg =
  let
    decoder : Json.Decoder GameState
    decoder = Json.object1 (\_ -> Draw) ("draw" := Json.bool)
  in
    Json.decodeString decoder msg

decodePlaying : String -> Result String GameState
decodePlaying msg =
  let
    decoder : Json.Decoder GameState
    decoder = Json.object1 PlayingGame ("playing" := modelDecoder)
  in
    Json.decodeString decoder msg

whichDecoder : Json.Decoder WhichPlayer
whichDecoder =
  let
    makeWhich : String -> WhichPlayer
    makeWhich s =
      case s of
      "pa" ->
        PlayerA
      "pb" ->
        PlayerB
      otherwise ->
        Debug.crash ("Invalid player " ++ s)
  in
    Json.object1 makeWhich Json.string

modelDecoder : Json.Decoder Model
modelDecoder =
  let
    cardDecoder : Json.Decoder Card
    cardDecoder =
      Json.object3 Card
        ("name" := Json.string)
        ("desc" := Json.string)
        ("imageURL" := Json.string)
    stackCardDecoder : Json.Decoder StackCard
    stackCardDecoder =
      Json.object2 StackCard
        ("owner" := whichDecoder)
        ("card" := cardDecoder)
  in
    Json.object6 Model
      ("handPA" := Json.list cardDecoder)
      ("handPB" := Json.list cardDecoder)
      ("stack" := Json.list stackCardDecoder)
      ("turn" := whichDecoder)
      ("lifePA" := Json.int)
      ("lifePB" := Json.int)
