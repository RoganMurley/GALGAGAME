module GameState.State exposing (mouseDown, mouseUp, tick, update)

import Assets.State as Assets
import Assets.Types as Assets
import Audio.State exposing (playSound)
import Browser.Events exposing (Visibility(..))
import Chat.Types as Chat
import DeckBuilding.State as DeckBuilding
import Game.State exposing (bareContextInit)
import GameState.Decoders exposing (stateDecoder)
import GameState.Messages exposing (Msg(..))
import GameState.Types exposing (GameState(..))
import GameType exposing (GameType(..))
import Json.Decode as Json
import Main.Messages as Main
import Main.Types exposing (Flags)
import Mode exposing (Mode)
import Mouse exposing (Position)
import PlayState.State as PlayState
import PlayState.Types exposing (PlayState)
import Ports exposing (log)
import Waiting.State as Waiting


update : Msg -> GameState -> Flags -> Mode -> GameType -> Assets.Model -> ( GameState, Cmd Main.Msg )
update msg state flags mode _ assets =
    case msg of
        PlayStateMsg playStateMsg ->
            case state of
                Started playState ->
                    let
                        ( newPlayState, cmd ) =
                            PlayState.update playStateMsg playState mode assets
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

                cmd =
                    case flags.visibility of
                        Visible ->
                            Cmd.none

                        Hidden ->
                            Cmd.batch
                                [ Ports.setTitle "🔔GALGA"
                                , playSound assets.audio "sfx/notify.mp3"
                                ]
            in
            case result of
                Ok playState ->
                    ( Started playState, cmd )

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


mouseDown : Position -> GameState -> Flags -> Mode -> GameType -> Assets.Model -> ( GameState, Cmd Main.Msg )
mouseDown mousePos state flags mode gameType assets =
    case state of
        Selecting selecting ->
            let
                ( newSelecting, cmd ) =
                    DeckBuilding.mouseDown mousePos selecting
            in
            ( Selecting newSelecting, cmd )

        Started playState ->
            let
                ( newPlayState, cmd ) =
                    PlayState.mouseDown flags assets gameType mode mousePos playState
            in
            ( Started newPlayState, cmd )

        _ ->
            ( state, Cmd.none )


mouseUp : Position -> GameState -> Flags -> Mode -> GameType -> Assets.Model -> ( GameState, Cmd Main.Msg )
mouseUp pos state flags mode gameType assets =
    case state of
        Started playState ->
            let
                ( newPlayState, cmd ) =
                    PlayState.mouseUp flags assets gameType mode pos playState
            in
            ( Started newPlayState, cmd )

        _ ->
            ( state, Cmd.none )


carry : GameState -> GameState -> GameState
carry old new =
    case old of
        Selecting { character, runes, runeSelect, ready, bounceTick, vfx, buttons } ->
            case new of
                Selecting selecting ->
                    Selecting { selecting | character = character, runes = runes, runeSelect = runeSelect, ready = ready, bounceTick = bounceTick, vfx = vfx, buttons = buttons }

                Started started ->
                    Started <|
                        PlayState.map (\game -> { game | vfx = vfx }) started

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


tick : Flags -> GameState -> Chat.Model -> GameType -> Float -> ( GameState, Cmd Msg )
tick flags state chat gameType dt =
    case state of
        Waiting waiting ->
            ( Waiting <| Waiting.tick dt waiting, Cmd.none )

        Selecting selecting ->
            let
                ctx =
                    bareContextInit flags.dimensions Assets.init flags.mouse

                newSelecting =
                    DeckBuilding.tick ctx dt selecting
            in
            ( Selecting newSelecting, Cmd.none )

        Started playState ->
            let
                ( newState, cmd ) =
                    PlayState.tick flags playState chat gameType dt
            in
            ( Started newState, Cmd.map PlayStateMsg cmd )
