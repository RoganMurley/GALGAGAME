module GameState.State exposing (mouseDown, mouseUp, tick, update)

import Assets.State as Assets
import Assets.Types as Assets
import Audio.State exposing (playSound)
import Browser.Events exposing (Visibility(..))
import Chat.Types as Chat
import Connected.Messages as Connected
import DeckBuilding.State as DeckBuilding
import Game.State exposing (bareContextInit)
import Game.Types as Game
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
import PlayState.Types exposing (PlayState(..))
import Players exposing (Players)
import Ports exposing (log)
import Random
import Room.Messages as Room
import Tutorial
import Waiting.State as Waiting


update : Msg -> GameState -> Flags -> Mode -> GameType -> Players -> Assets.Model -> ( GameState, Cmd Main.Msg )
update msg state flags mode _ players assets =
    case msg of
        PlayStateMsg playStateMsg ->
            case state of
                Started playState ->
                    let
                        ( newPlayState, cmd ) =
                            PlayState.update playStateMsg playState mode players assets
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

        Sync str tags ->
            let
                result : Result Json.Error GameState
                result =
                    Json.decodeString stateDecoder str
            in
            case result of
                Ok newState ->
                    ( hydrateSeed flags.seed <| applyTags tags <| carry state newState, Cmd.none )

                Err err ->
                    ( state, log <| Json.errorToString err )

        SelectingMsg selectMsg ->
            case state of
                Selecting m ->
                    let
                        ( newModel, cmd ) =
                            DeckBuilding.update selectMsg m assets players
                    in
                    ( Selecting newModel, cmd )

                _ ->
                    ( state, log "Expected a Selecting state" )


mouseDown : Position -> GameState -> Flags -> Mode -> GameType -> Players -> Assets.Model -> ( GameState, Cmd Main.Msg )
mouseDown mousePos state flags mode gameType players assets =
    case state of
        Waiting waiting ->
            ( Waiting <| Waiting.mouseDown flags assets mousePos waiting, Cmd.none )

        Selecting selecting ->
            let
                ( newSelecting, cmd ) =
                    DeckBuilding.mouseDown mousePos players assets selecting
            in
            ( Selecting newSelecting, cmd )

        Started playState ->
            let
                ( newPlayState, cmd ) =
                    PlayState.mouseDown flags assets gameType mode players mousePos playState
            in
            ( Started newPlayState, cmd )


mouseUp : Position -> GameState -> Flags -> Mode -> GameType -> Players -> Assets.Model -> ( GameState, Cmd Main.Msg )
mouseUp pos state flags mode gameType players assets =
    case state of
        Started playState ->
            let
                ( newPlayState, cmd ) =
                    PlayState.mouseUp flags assets gameType mode players pos playState
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
                    Selecting
                        { selecting
                            | character = character
                            , runes = runes
                            , runeSelect = runeSelect
                            , ready = ready
                            , bounceTick = bounceTick
                            , vfx = vfx
                            , buttons = buttons
                        }

                Started started ->
                    Started <|
                        PlayState.map
                            (\game -> { game | vfx = vfx })
                            started

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


tick : Flags -> GameState -> Chat.Model -> GameType -> Float -> ( GameState, Cmd Main.Msg )
tick flags state chat gameType dt =
    case state of
        Waiting waiting ->
            let
                ( newWaiting, msg ) =
                    Waiting.tick dt waiting
            in
            ( Waiting newWaiting, msg )

        Selecting selecting ->
            let
                ctx =
                    bareContextInit flags.dimensions Assets.init flags.mouse

                ( newSelecting, cmd ) =
                    DeckBuilding.tick ctx dt chat selecting
            in
            ( Selecting newSelecting, Cmd.map (Main.RoomMsg << Room.ConnectedMsg << Connected.GameStateMsg << SelectingMsg) cmd )

        Started playState ->
            let
                ( newState, cmd ) =
                    PlayState.tick flags playState chat gameType dt
            in
            ( Started newState, Cmd.map (Main.RoomMsg << Room.ConnectedMsg << Connected.GameStateMsg << PlayStateMsg) cmd )


applyTags : List String -> GameState -> GameState
applyTags tags state =
    List.foldr
        (\f -> f tags)
        state
        [ applyTagsTutorial0
        , applyTagsTutorial1
        , applyTagsTutorial2
        , applyTagsPassive
        , applyTagsCpu
        ]


applyTagsTutorial0 : List String -> GameState -> GameState
applyTagsTutorial0 =
    applyTagGame
        "tutorial-0"
        (\game -> { game | tutorial = Tutorial.beginStageBasic })


applyTagsTutorial1 : List String -> GameState -> GameState
applyTagsTutorial1 =
    applyTagGame
        "tutorial-1"
        (\game -> { game | tutorial = Tutorial.beginStageA })


applyTagsTutorial2 : List String -> GameState -> GameState
applyTagsTutorial2 =
    applyTagGame
        "tutorial-2"
        (\game -> { game | tutorial = Tutorial.beginStageB })


applyTagsPassive : List String -> GameState -> GameState
applyTagsPassive =
    applyTagGame
        "passive"
        (\game -> { game | passive = True })


applyTagsCpu : List String -> GameState -> GameState
applyTagsCpu =
    applyTagGame
        "cpu"
        (\game -> { game | cpu = True })


applyTagGame : String -> (Game.Model -> Game.Model) -> (List String -> GameState -> GameState)
applyTagGame tag f =
    \tags state ->
        if List.any ((==) tag) tags then
            case state of
                Started (Playing playState) ->
                    Started <|
                        Playing
                            { playState | game = f playState.game }

                _ ->
                    state

        else
            state


hydrateSeed : Int -> GameState -> GameState
hydrateSeed seed state =
    case state of
        Waiting waiting ->
            Waiting { waiting | seed = Just <| Random.initialSeed seed }

        _ ->
            state
