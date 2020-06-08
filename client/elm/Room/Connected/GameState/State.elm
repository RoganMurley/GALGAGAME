module GameState.State exposing (tick, update)

import DeckBuilding.State as DeckBuilding
import Game.State exposing (bareContextInit)
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
import Texture.State as Texture


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
                            DeckBuilding.update selectMsg m
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
        Selecting { characters, runes, runeSelect, ready } ->
            case new of
                Selecting selecting ->
                    Selecting { selecting | characters = characters, runes = runes, runeSelect = runeSelect, ready = ready }

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
        Selecting selecting ->
            let
                ctx =
                    bareContextInit flags.dimensions Texture.init

                newSelecting =
                    DeckBuilding.tick ctx   dt selecting
            in
            ( Selecting newSelecting, Cmd.none )

        Started playState ->
            let
                ( newState, cmd ) =
                    PlayState.tick flags playState dt
            in
            ( Started newState, Cmd.map PlayStateMsg cmd )

        _ ->
            ( state, Cmd.none )
