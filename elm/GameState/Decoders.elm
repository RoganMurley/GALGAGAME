module GameState.Decoders exposing (decodeState, resDecoder)

import Json.Decode as Json exposing (Decoder, fail, field, index, int, list, maybe, string, succeed)
import Card.Types exposing (Card)
import CharacterSelect.Types as CharacterSelect
import GameState.Types exposing (GameState(..), fullify, unfullify)
import Model.Types exposing (..)
import Util exposing (fromJust)


decodeState : String -> GameState -> GameState
decodeState msg oldState =
    case Json.decodeString (stateDecoder oldState) msg of
        Ok result ->
            result

        Err err ->
            Debug.crash err


stateDecoder : GameState -> Decoder GameState
stateDecoder oldState =
    Json.oneOf
        [ waitingDecoder
        , selectingDecoder oldState
        , playingDecoder oldState
        , endedDecoder
        ]


waitingDecoder : Decoder GameState
waitingDecoder =
    Json.map (\_ -> Waiting) <| field "waiting" Json.bool


selectingDecoder : GameState -> Decoder GameState
selectingDecoder oldState =
    let
        characterDecoder : Decoder CharacterSelect.Character
        characterDecoder =
            Json.map3 CharacterSelect.Character
                (field "name" string)
                (field "img_url" string)
                (field "cards" characterCardsDecoder)

        characterCardsDecoder : Decoder ( Card, Card, Card, Card )
        characterCardsDecoder =
            Json.map4 (,,,)
                (index 0 cardDecoder)
                (index 1 cardDecoder)
                (index 2 cardDecoder)
                (index 3 cardDecoder)

        makeSelectState : List CharacterSelect.Character -> List CharacterSelect.Character -> GameState
        makeSelectState selecting selected =
            Selecting (CharacterSelect.Model selecting selected (hoverCharacter (fromJust (List.head selecting))))

        hoverCharacter : CharacterSelect.Character -> CharacterSelect.Character
        hoverCharacter default =
            case oldState of
                Selecting { hover } ->
                    hover

                otherwise ->
                    default
    in
        Json.map2 makeSelectState
            (field "selecting" <| list characterDecoder)
            (field "selected" <| list characterDecoder)


endedDecoder : Decoder GameState
endedDecoder =
    Json.map (\w -> Ended w Nothing ( [], 0 ))
        (field "winner" <| maybe whichDecoder)


playingDecoder : GameState -> Decoder GameState
playingDecoder oldState =
    Json.map (\a -> PlayingGame (fullify a { diffOtherLife = 0, diffLife = 0 }) ( [], 0 ))
        (field "playing" <| modelDecoder oldState)


whichDecoder : Decoder WhichPlayer
whichDecoder =
    let
        decode : String -> Decoder WhichPlayer
        decode s =
            case s of
                "pa" ->
                    succeed PlayerA

                "pb" ->
                    succeed PlayerB

                otherwise ->
                    fail ("Invalid player " ++ s)
    in
        string |> Json.andThen decode


cardDecoder : Decoder Card
cardDecoder =
    Json.map4 Card
        (field "name" string)
        (field "desc" string)
        (field "imageURL" string)
        (field "sfxURL" string)


modelDecoder : GameState -> Decoder Model
modelDecoder oldState =
    let
        stackCardDecoder : Decoder StackCard
        stackCardDecoder =
            Json.map2 StackCard
                (field "owner" whichDecoder)
                (field "card" cardDecoder)
    in
        Json.map6 (\a b c d e f -> Model a b c d e f Nothing)
            (field "handPA" <| list cardDecoder)
            (field "handPB" int)
            (field "stack" <| list stackCardDecoder)
            (field "turn" whichDecoder)
            (field "lifePA" int)
            (field "lifePB" int)


resDecoder : GameState -> Decoder ( GameState, List Model )
resDecoder oldState =
    Json.map2 (,)
        (field "final" <| stateDecoder oldState)
        (field "list" <| list <| modelDecoder oldState)
