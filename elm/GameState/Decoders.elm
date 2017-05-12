module GameState.Decoders exposing (decodeState, resDecoder)

import Json.Decode as Json exposing (Decoder, fail, field, index, int, list, maybe, string, succeed)
import Card.Decoders as Card
import Card.Types exposing (Card)
import CharacterSelect.Types as CharacterSelect
import GameState.Types exposing (GameState(..))
import Model.Decoders exposing (modelDecoder, whichDecoder)
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
        , playingDecoder
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
                (index 0 Card.decoder)
                (index 1 Card.decoder)
                (index 2 Card.decoder)
                (index 3 Card.decoder)

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


playingDecoder : Decoder GameState
playingDecoder =
    Json.map (\m -> PlayingGame m ( [], 0 ))
        (field "playing" <| modelDecoder)


resDecoder : GameState -> Decoder ( GameState, List Model )
resDecoder oldState =
    Json.map2 (,)
        (field "final" <| stateDecoder oldState)
        (field "list" <| list <| modelDecoder)
