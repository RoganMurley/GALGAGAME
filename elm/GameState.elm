module GameState exposing (Card, Hand, Model, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))

import Messages exposing (GameMsg(Sync), Msg(DrawCard))


-- TYPES.

type alias Model =
  {
    hand : Hand
  , otherHand : Hand
  , stack : PlayStack
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

-- INITIAL MODEL.

init : Model
init =
  {
    hand = []
  , otherHand = []
  , stack = []
  }


-- VIEWS.

view : Model -> Html Msg
view model =
  div []
    [
      viewOtherHand model.otherHand
    , viewHand model.hand
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


-- UPDATE
update : GameMsg -> Model -> Model
update msg model =
  case msg of
    Sync str ->
      syncHands model str

syncHands : Model -> String -> Model
syncHands model msg =
  let
    result : Result String (Hand, Hand)
    result = decodeHands msg
  in
    case result of
      Ok (paHand, pbHand) ->
        { model | hand = paHand, otherHand = pbHand }
      Err err ->
        Debug.crash ("Sync hand error: " ++ err)

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
