module GameState.State exposing (tick, update)

import CharacterSelect.State as CharacterSelect
import GameState.Decoders exposing (stateDecoder)
import GameState.Messages exposing (Msg(..))
import GameState.Types exposing (GameState(..))
import Json.Decode as Json
import Main.Messages as Main
import Main.Types exposing (Flags)
import Mode exposing (Mode)
import PlayState.State as PlayState
import PlayState.Types exposing (PlayState)
import Ports exposing (log)


update : Msg -> GameState -> Mode -> ( GameState, Cmd Main.Msg )
update msg state mode =
    case msg of
        Mouse pos ->
            case state of
                Started playState ->
                    ( Started <| PlayState.mouseMove (Just pos) playState, Cmd.none )

                _ ->
                    ( state, Cmd.none )

        MouseClick pos ->
            case state of
                Started playState ->
                    let
                        ( newPlayState, cmd ) =
                            PlayState.mouseClick mode pos playState
                    in
                    ( Started newPlayState, cmd )

                _ ->
                    ( state, Cmd.none )

        PlayStateMsg playStateMsg ->
            case state of
                Started playState ->
                    let
                        ( newPlayState, cmd ) =
                            PlayState.update playStateMsg playState mode
                    in
                    ( Started newPlayState, cmd )

                _ ->
                    ( state, log "Expected a Started state" )

        ResolveOutcome str ->
            let
                oldPlayState : Maybe PlayState
                oldPlayState =
                    case state of
                        Started playState ->
                            Just playState

                        _ ->
                            Nothing

                result : Result Json.Error PlayState
                result =
                    PlayState.resolveOutcomeStr str oldPlayState
            in
            case result of
                Ok playState ->
                    ( Started playState, Cmd.none )

                Err err ->
                    ( state, log <| Json.errorToString err )

        Sync str ->
            let
                result : Result Json.Error GameState
                result =
                    Json.decodeString stateDecoder str
            in
            case result of
                Ok newState ->
                    ( carry state newState, Cmd.none )

                Err err ->
                    ( state, log <| Json.errorToString err )

        SelectingMsg selectMsg ->
            case state of
                Selecting m ->
                    let
                        ( newModel, cmd ) =
                            CharacterSelect.update selectMsg m
                    in
                    ( Selecting newModel, cmd )

                _ ->
                    ( state, log "Expected a Selecting state" )

        Touch pos ->
            case state of
                Started playState ->
                    ( Started <| PlayState.mouseMove pos playState, Cmd.none )

                _ ->
                    ( state, Cmd.none )


carry : GameState -> GameState -> GameState
carry old new =
    case old of
        Selecting { vm } ->
            case new of
                Selecting selecting ->
                    Selecting { selecting | vm = vm }

                _ ->
                    new

        Started oldStarted ->
            case new of
                Started newStarted ->
                    Started <|
                        PlayState.carry oldStarted newStarted

                _ ->
                    new

        _ ->
            new


tick : Flags -> GameState -> Float -> ( GameState, Cmd Msg )
tick flags state dt =
    case state of
        Started playState ->
            let
                ( newState, cmd ) =
                    PlayState.tick flags playState dt
            in
            ( Started newState, Cmd.map PlayStateMsg cmd )

        _ ->
            ( state, Cmd.none )
