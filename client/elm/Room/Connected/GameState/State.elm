module GameState.State exposing (update, tick)

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


update : Msg -> GameState -> Mode -> Flags -> ( GameState, Cmd Main.Msg )
update msg state mode flags =
    case msg of
        Mouse pos ->
            case state of
                Started playState ->
                    ( Started <| PlayState.mouseMove pos playState
                    , Cmd.none
                    )

                _ ->
                    ( state, Cmd.none )

        MouseClick pos ->
            case state of
                Started playState ->
                    let
                        ( newPlayState, cmd ) =
                            PlayState.mouseClick mode flags pos playState
                    in
                        ( Started newPlayState, cmd )

                _ ->
                    ( state, Cmd.none )

        PlayStateMsg playStateMsg ->
            case state of
                Started playState ->
                    let
                        ( newPlayState, cmd ) =
                            PlayState.update playStateMsg playState mode flags
                    in
                        ( Started newPlayState, cmd )

                _ ->
                    Debug.log
                        "Expected a Started state"
                        ( state, Cmd.none )

        ResolveOutcome str ->
            let
                oldPlayState : Maybe PlayState
                oldPlayState =
                    case state of
                        Started playState ->
                            Just playState

                        _ ->
                            Nothing
            in
                ( Started <| PlayState.resolveOutcome str oldPlayState
                , Cmd.none
                )

        Sync str ->
            ( syncState state str, Cmd.none )

        SelectingMsg selectMsg ->
            case state of
                Selecting m ->
                    let
                        ( newModel, cmd ) =
                            CharacterSelect.update selectMsg m
                    in
                        ( Selecting newModel, cmd )

                _ ->
                    Debug.log
                        "Expected a Selecting state"
                        ( state, Cmd.none )


syncState : GameState -> String -> GameState
syncState oldState msg =
    case Json.decodeString stateDecoder msg of
        Ok newState ->
            carryVm oldState newState

        Err err ->
            Debug.log err oldState


carryVm : GameState -> GameState -> GameState
carryVm old new =
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
                        PlayState.carryVm oldStarted newStarted

                _ ->
                    new

        _ ->
            new


tick : Flags -> GameState -> Float -> GameState
tick flags state dt =
    case state of
        Started playState ->
            Started <| PlayState.tick flags playState dt

        _ ->
            state
