module GameState.Decoders exposing (..)

import Json.Decode as Json exposing (Decoder, fail, field, index, list, maybe, string, succeed)
import Card.Decoders as Card
import Card.Types exposing (Card)
import CharacterSelect.Character as CharacterSelect
import CharacterSelect.ViewModel
import Clock.State exposing (clockInit)
import GameState.Types exposing (GameState(..), PlayState(..), WaitType(..), Winner)
import Model.Decoders as Model
import Model.Types exposing (Model)
import Resolvable.State as Resolvable
import WhichPlayer.Decoders as WhichPlayer


stateDecoder : Decoder GameState
stateDecoder =
    Json.oneOf
        [ waitingDecoder
        , selectingDecoder
        , Json.map Started playStateDecoder
        ]


playStateDecoder : Decoder PlayState
playStateDecoder =
    Json.oneOf
        [ playingDecoder
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

                _ ->
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


endedDecoder : Decoder PlayState
endedDecoder =
    let
        endedInit : Winner -> Model -> PlayState
        endedInit w m =
            Ended w (clockInit <| Resolvable.init m []) Nothing
    in
        Json.map2 endedInit
            (field "winner" <| maybe WhichPlayer.decoder)
            (field "final" <| Model.decoder)


playingDecoder : Decoder PlayState
playingDecoder =
    let
        playingInit : Model -> PlayState
        playingInit m =
            Playing (clockInit <| Resolvable.init m [])
    in
        Json.map playingInit
            (field "playing" <| Model.decoder)


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
