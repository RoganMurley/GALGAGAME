module GameState exposing (Card, Hand, Model, init, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Messages exposing (..)


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
