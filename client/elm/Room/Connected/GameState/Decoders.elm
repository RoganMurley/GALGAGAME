module GameState.Decoders exposing (decodeState, resDecoder)

import Json.Decode as Json exposing (Decoder, fail, field, index, int, list, maybe, string, succeed)
import Card.Decoders as Card
import Card.Types exposing (Card)
import CharacterSelect.Character as CharacterSelect
import CharacterSelect.ViewModel
import GameState.Types exposing (GameState(..), WaitType(..))
import Model.Decoders exposing (modelDecoder, whichDecoder)
import Model.Types exposing (..)
import Model.ViewModel


decodeState : String -> Result String GameState
decodeState msg =
    Json.decodeString stateDecoder msg


stateDecoder : Decoder GameState
stateDecoder =
    Json.oneOf
        [ waitingDecoder
        , selectingDecoder
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


selectingDecoder : Decoder GameState
selectingDecoder =
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

        makeSelectState : List CharacterSelect.Character -> List CharacterSelect.Character -> Result String GameState
        makeSelectState characters selected =
            case List.head characters of
                Nothing ->
                    Err "No characters in list"

                Just initialHover ->
                    Ok <|
                        Selecting
                            { characters = characters
                            , selected = selected
                            , vm =
                                CharacterSelect.ViewModel.init initialHover
                            }
    in
        collapseResults <|
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


resDecoder : Decoder ( GameState, List Model )
resDecoder =
    Json.map2 (,)
        (field "final" <| stateDecoder)
        (field "list" <| list <| modelDecoder)


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
