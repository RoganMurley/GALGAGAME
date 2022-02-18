module GameState.Decoders exposing (collapseResults, selectingDecoder, stateDecoder)

import DeckBuilding.Decoders
import DeckBuilding.State as DeckBuilding
import DeckBuilding.Types exposing (ChoosingCharacter(..))
import GameState.Types exposing (GameState(..))
import Json.Decode as Json exposing (Decoder, fail, field, list, succeed)
import PlayState.Decoders as PlayState
import RuneSelect.Decoders
import RuneSelect.Types exposing (Rune)
import Waiting.Decoders as Waiting
import Waiting.Types exposing (WaitType(..))


stateDecoder : Decoder GameState
stateDecoder =
    Json.oneOf
        [ Json.map Waiting Waiting.decoder
        , selectingDecoder
        , Json.map Started PlayState.decoder
        ]


selectingDecoder : Decoder GameState
selectingDecoder =
    let
        makeSelectState : ChoosingCharacter -> List Rune -> GameState
        makeSelectState choosingCharacter runes =
            Selecting <|
                case choosingCharacter of
                    ChosenCharacter character ->
                        DeckBuilding.init True (Just character) runes

                    UnchosenCharacter character ->
                        DeckBuilding.init False character runes
    in
    Json.map2 makeSelectState
        (field "character" DeckBuilding.Decoders.choosingCharacter)
        (field "all_runes" <| list RuneSelect.Decoders.rune)


collapseResults : Decoder (Result String a) -> Decoder a
collapseResults decoder =
    let
        collapse : Result String a -> Decoder a
        collapse x =
            case x of
                Err err ->
                    fail err

                Ok ok ->
                    succeed ok
    in
    decoder |> Json.andThen collapse
