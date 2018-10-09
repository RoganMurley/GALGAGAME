module GameState.Decoders exposing (..)

import Json.Decode as Json exposing (Decoder, fail, field, list, maybe, string, succeed)
import CharacterSelect.Decoders
import CharacterSelect.State as CharacterSelect
import CharacterSelect.Types exposing (Character)
import Game.State exposing (gameInit)
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
                    fail <| "Invalid WaitType " ++ s
    in
        Json.map Waiting
            (field "waiting" (string |> Json.andThen decode))


selectingDecoder : Decoder GameState
selectingDecoder =
    let
        makeSelectState : List Character -> List Character -> Result String GameState
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
                                CharacterSelect.viewModelInit initialHover
                            }
    in
        collapseResults <|
            Json.map2 makeSelectState
                (field "selecting" <| list CharacterSelect.Decoders.character)
                (field "selected" <| list CharacterSelect.Decoders.character)


endedDecoder : Decoder PlayState
endedDecoder =
    let
        endedInit : Winner -> Model -> PlayState
        endedInit w m =
            Ended w (gameInit <| Resolvable.init m []) Nothing
    in
        Json.map2 endedInit
            (field "winner" <| maybe WhichPlayer.decoder)
            (field "final" <| Model.decoder)


playingDecoder : Decoder PlayState
playingDecoder =
    let
        playingInit : Model -> PlayState
        playingInit m =
            Playing <| gameInit <| Resolvable.init m []
    in
        Json.map playingInit <|
            field "playing" <|
                Model.decoder


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
