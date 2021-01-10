module GameState.Decoders exposing (collapseResults, selectingDecoder, stateDecoder, waitingDecoder)

import DeckBuilding.Decoders
import DeckBuilding.State as DeckBuilding
import DeckBuilding.Types exposing (Character)
import GameState.Types exposing (GameState(..), WaitType(..))
import Json.Decode as Json exposing (Decoder, fail, field, list, maybe, string, succeed)
import PlayState.Decoders as PlayState
import RuneSelect.Decoders
import RuneSelect.Types exposing (Rune)


stateDecoder : Decoder GameState
stateDecoder =
    Json.oneOf
        [ waitingDecoder
        , selectingDecoder
        , Json.map Started PlayState.decoder
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

                _ ->
                    fail <| "Invalid WaitType " ++ s
    in
    Json.map Waiting
        (field "waiting" (string |> Json.andThen decode))


selectingDecoder : Decoder GameState
selectingDecoder =
    let
        makeSelectState : Maybe Character -> List Character -> List Rune -> Result String GameState
        makeSelectState selectedCharacter characters runes =
            case characters of
                character :: remaining ->
                    Ok <|
                        Selecting <|
                            case selectedCharacter of
                                Just selected ->
                                    DeckBuilding.init True selected characters runes

                                Nothing ->
                                    DeckBuilding.init False character remaining runes

                _ ->
                    Err "No characters"
    in
    collapseResults <|
        Json.map3 makeSelectState
            (field "character" <| maybe DeckBuilding.Decoders.character)
            (field "all_characters" <| list DeckBuilding.Decoders.character)
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
