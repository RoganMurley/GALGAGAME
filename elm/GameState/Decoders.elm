module GameState.Decoders exposing (decodeState, resDecoder)

import Json.Decode as Json exposing (field, maybe)
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


stateDecoder : GameState -> Json.Decoder GameState
stateDecoder oldState =
    Json.oneOf
        [ waitingDecoder
        , selectingDecoder oldState
        , playingDecoder oldState
        , endedDecoder
        ]


waitingDecoder : Json.Decoder GameState
waitingDecoder =
    Json.map (\_ -> Waiting) (field "waiting" Json.bool)


selectingDecoder : GameState -> Json.Decoder GameState
selectingDecoder oldState =
    let
        characterDecoder : Json.Decoder CharacterSelect.Character
        characterDecoder =
            Json.map3 CharacterSelect.Character
                (field "name" Json.string)
                (field "img_url" Json.string)
                (field "cards" characterCardsDecoder)

        characterCardsDecoder : Json.Decoder ( Card, Card, Card, Card )
        characterCardsDecoder =
            Json.map4 (,,,)
                (Json.index 0 cardDecoder)
                (Json.index 1 cardDecoder)
                (Json.index 2 cardDecoder)
                (Json.index 3 cardDecoder)

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
            (field "selecting" (Json.list characterDecoder))
            (field "selected" (Json.list characterDecoder))


endedDecoder : Json.Decoder GameState
endedDecoder =
    Json.map (\w -> Ended w Nothing ( [], 0 ))
        (field "winner" (maybe whichDecoder))


playingDecoder : GameState -> Json.Decoder GameState
playingDecoder oldState =
    Json.map (\a -> PlayingGame (fullify a { diffOtherLife = 0, diffLife = 0 }) ( [], 0 ))
        (field "playing" (modelDecoder oldState))


whichDecoder : Json.Decoder WhichPlayer
whichDecoder =
    let
        decode : String -> Json.Decoder WhichPlayer
        decode s =
            case s of
                "pa" ->
                    Json.succeed PlayerA

                "pb" ->
                    Json.succeed PlayerB

                otherwise ->
                    Json.fail ("Invalid player " ++ s)
    in
        Json.string |> Json.andThen decode


cardDecoder : Json.Decoder Card
cardDecoder =
    Json.map4 Card
        (field "name" Json.string)
        (field "desc" Json.string)
        (field "imageURL" Json.string)
        (field "sfxURL" Json.string)


modelDecoder : GameState -> Json.Decoder Model
modelDecoder oldState =
    let
        stackCardDecoder : Json.Decoder StackCard
        stackCardDecoder =
            Json.map2 StackCard
                (field "owner" whichDecoder)
                (field "card" cardDecoder)
    in
        Json.map6 (\a b c d e f -> Model a b c d e f Nothing)
            (field "handPA" <| Json.list cardDecoder)
            (field "handPB" Json.int)
            (field "stack" <| Json.list stackCardDecoder)
            (field "turn" whichDecoder)
            (field "lifePA" Json.int)
            (field "lifePB" Json.int)


resDecoder : GameState -> Json.Decoder ( GameState, List Model )
resDecoder oldState =
    Json.map2 (,)
        (field "final" <| stateDecoder oldState)
        (field "list" <| Json.list <| modelDecoder oldState)
