module GameState.State exposing (tick, update)

import Assets.State as Assets
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
import PlayState.State as PlayState
import PlayState.Types exposing (PlayState)
import Ports exposing (log)


update : Msg -> GameState -> Flags -> Mode -> ( GameState, Cmd Main.Msg )
update msg state flags mode =
    case msg of
        MouseClick pos ->
            case state of
                Selecting selecting ->
                    let
                        ( newSelecting, cmd ) =
                            DeckBuilding.mouseClick pos selecting
                    in
                    ( Selecting newSelecting, cmd )

                Started playState ->
                    let
                        ( newPlayState, cmd ) =
                            PlayState.mouseClick flags mode pos playState
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


carry : GameState -> GameState -> GameState
carry old new =
    case old of
        Selecting { characters, runes, runeSelect, ready, bounceTick, vfx, buttons } ->
            case new of
                Selecting selecting ->
                    Selecting { selecting | characters = characters, runes = runes, runeSelect = runeSelect, ready = ready, bounceTick = bounceTick, vfx = vfx, buttons = buttons }

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


tick : Flags -> GameType -> GameState -> Float -> ( GameState, Cmd Msg )
tick flags gameType state dt =
    case state of
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
                    PlayState.tick flags (Just gameType) playState dt
            in
            ( Started newState, Cmd.map PlayStateMsg cmd )

        _ ->
            ( state, Cmd.none )
