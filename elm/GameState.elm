module GameState exposing (Card, Hand, Model, Turn(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))

import Messages exposing (GameMsg(Sync), Msg(DrawCard, EndTurn))


-- TYPES.

type alias Model =
  {
    hand : Hand
  , otherHand : Hand
  , stack : PlayStack
  , turn : Turn
  }

type alias Hand =
  List Card

type alias PlayStack =
  List Card

type alias Card =
  {
    name : String
  , desc : String
  , imgURL: String
  , cardColor : String
  }

type Turn
  = PlayerA
  | PlayerB

-- INITIAL MODEL.

init : Model
init =
  {
    hand = []
  , otherHand = []
  , stack = []
  , turn = PlayerA
  }


-- VIEWS.
view : Model -> Html Msg
view model =
  div []
    [
      viewOtherHand model.otherHand
    , viewHand model.hand
    , viewTurn model.turn
    ]

viewHand : Hand -> Html Msg
viewHand hand =
  let
    viewCard : Card -> Html Msg
    viewCard { name, desc, imgURL, cardColor } = div
      [
        class "card my-card"
      , onClick DrawCard
      , style [ ("background-color", cardColor) ]
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
      div [ class "turn-indi enemy-turn" ] [ text "Enemy Turn" ]


-- UPDATE
update : GameMsg -> Model -> Model
update msg model =
  case msg of
    Sync str ->
      syncModel model str

syncModel : Model -> String -> Model
syncModel model msg =
  let
    result : Result String (Turn, Hand, Hand)
    result = decodeState msg
  in
    case result of
      Ok (turn, paHand, pbHand) ->
        { model | turn = turn, hand = paHand, otherHand = pbHand }
      Err err ->
        Debug.crash ("Sync hand error: " ++ err)

decodeState : String -> Result String (Turn, Hand, Hand)
decodeState msg =
  let
    result : Result String (String, Hand, Hand)
    result = Json.decodeString handDecoder msg
    handDecoder : Json.Decoder (String, Hand, Hand)
    handDecoder =
      Json.object3 (\a b c -> (a, b, c))
        ("turn" := Json.string)
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
    case result of
      Ok ("pa", handPA, handPB) ->
        Ok (PlayerA, handPA, handPB)

      Ok ("pb", handPA, handPB) ->
        Ok (PlayerB, handPA, handPB)

      Ok (t, _, _) ->
        Err ("Invalid turn, should be pa or pb but is instead " ++ t)

      Err err ->
        Err err
