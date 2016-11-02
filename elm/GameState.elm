module GameState exposing (Card, Hand, Model, Turn, WhichPlayer(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))

import Messages exposing (GameMsg(Sync), Msg(DrawCard, EndTurn, PlayCard))


-- TYPES.

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
      div [ class "life-counter" ] [ text ((toString myLife) ++ " LP") ]
    , div [ class "life-counter" ] [ text ((toString otherLife) ++ " LP") ]
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
update : GameMsg -> Model -> Model
update msg model =
  case msg of
    Sync str ->
      syncModel model str

syncModel : Model -> String -> Model
syncModel model msg =
  let
    result : Result String (Turn, Hand, Hand, Life, Life, Stack)
    result = decodeState msg
  in
    case result of
      Ok (turn, paHand, pbHand, paLife, pbLife, stack) ->
        { model | turn = turn, hand = paHand, otherHand = pbHand, life = paLife, otherLife = pbLife, stack = stack }
      Err err ->
        Debug.crash ("Sync hand error: " ++ err)

decodeState : String -> Result String (Turn, Hand, Hand, Life, Life, Stack)
decodeState msg =
  let
    result : Result String (String, Hand, Hand, Life, Life, Stack)
    result = Json.decodeString handDecoder msg
    handDecoder : Json.Decoder (String, Hand, Hand, Life, Life, Stack)
    handDecoder =
      Json.object6 (\a b c d e f -> (a, b, c, d, e, f))
        ("turn" := Json.string)
        ("handPA" := Json.list cardDecoder)
        ("handPB" := Json.list cardDecoder)
        ("lifePA" := Json.int)
        ("lifePB" := Json.int)
        ("stack" := Json.list stackCardDecoder)
    cardDecoder : Json.Decoder Card
    cardDecoder =
      Json.object3 Card
        ("name" := Json.string)
        ("desc" := Json.string)
        ("imageURL" := Json.string)
    stackCardDecoder : Json.Decoder StackCard
    stackCardDecoder =
      Json.object2 makeStackCard
        ("owner" := Json.string)
        ("card" := cardDecoder)
    makeStackCard : String -> Card -> StackCard
    makeStackCard s c =
      case s of
        "pa" ->
          StackCard PlayerA c
        "pb" ->
          StackCard PlayerB c
        otherwise ->
          Debug.crash "Invalid stack card owner"
  in
    case result of
      Ok ("pa", handPA, handPB, lifePA, lifePB, stack) ->
        Ok (PlayerA, handPA, handPB, lifePA, lifePB, stack)

      Ok ("pb", handPA, handPB, lifePA, lifePB, stack) ->
        Ok (PlayerB, handPA, handPB, lifePA, lifePB, stack)

      Ok (t, _, _, _, _, _) ->
        Err ("Invalid turn, should be pa or pb but is instead " ++ t)

      Err err ->
        Err err
