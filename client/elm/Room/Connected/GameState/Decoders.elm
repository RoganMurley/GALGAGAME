module GameState.Decoders exposing (decodeState, resDecoder)

import Json.Decode as Json exposing (Decoder, fail, field, index, int, list, maybe, string, succeed)
import Card.Decoders as Card
import Card.Types exposing (Card)
import CharacterSelect.Types as CharacterSelect
import GameState.Types exposing (GameState(..), WaitType(..))
import Model.Decoders exposing (modelDecoder, whichDecoder)
import Model.Types exposing (..)
import Util exposing (fromJust)
import Model.ViewModel


decodeState : String -> GameState -> Result String GameState
decodeState msg oldState =
    Json.decodeString (stateDecoder oldState) msg


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
    let
        decode : String -> Decoder WaitType
        decode s =
            case s of
                "quickplay" ->
                    succeed WaitQuickplay

                "custom" ->
                    succeed WaitCustom

                otherwise ->
                    fail ("Invalid WaitType " ++ s)
    in
        Json.map Waiting
            (field "waiting" (string |> Json.andThen decode))


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
    Json.map2 (\w m -> Ended w m Model.ViewModel.init Nothing ( [], 0 ))
        (field "winner" <| maybe whichDecoder)
        (field "final" <| modelDecoder)


playingDecoder : Decoder GameState
playingDecoder =
    Json.map (\m -> PlayingGame ( m, Model.ViewModel.init ) ( [], 0 ))
        (field "playing" <| modelDecoder)


resDecoder : GameState -> Decoder ( GameState, List Model )
resDecoder oldState =
    Json.map2 (,)
        (field "final" <| stateDecoder oldState)
        (field "list" <| list <| modelDecoder)
