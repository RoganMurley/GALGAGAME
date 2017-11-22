module Connected.State exposing (init, update, receive, tick)

import Audio exposing (playSound, setVolume)
import Connected.Types exposing (..)
import Connected.Decoders exposing (decodeHoverOutcome, decodePlayers)
import Connected.Messages exposing (..)
import GameState.Messages as GameState
import GameState.State as GameState
import GameState.Types exposing (..)
import Settings.State as Settings
import Settings.Messages as Settings
import Main.Messages as Main
import Main.Types exposing (Flags)
import Mode exposing (Mode(..))
import Util exposing (message, send, splitOn)


init : Mode -> String -> Model
init mode roomID =
    { game = Waiting WaitQuickplay
    , settings = Settings.init
    , mode = mode
    , roomID = roomID
    , players = ( Nothing, Nothing )
    }


update : Flags -> Msg -> Model -> ( Model, Cmd Main.Msg )
update ({ hostname } as flags) msg ({ game, settings, mode } as model) =
    case msg of
        GameStateMsg gameMsg ->
            let
                ( newGame, cmd ) =
                    GameState.update gameMsg game mode flags
            in
                ( { model | game = newGame }, cmd )

        SettingsMsg settingsMsg ->
            ( { model | settings = Settings.update settingsMsg settings }
            , Cmd.none
            )

        Concede ->
            ( { model
                | settings = Settings.update Settings.CloseSettings settings
              }
            , send hostname "concede:"
            )

        SetVolume volume ->
            let
                newVolume =
                    clamp 0 100 volume
            in
                ( { model
                    | settings =
                        { settings | volume = newVolume }
                  }
                , setVolume newVolume
                )


tick : Model -> Float -> Model
tick model dt =
    { model | game = GameState.tick model.game dt }


receive : Model -> String -> Flags -> ( Model, Cmd Main.Msg )
receive ({ mode } as model) msg flags =
    let
        ( command, content ) =
            splitOn ":" msg
    in
        case command of
            "sync" ->
                let
                    ( newGame, cmd ) =
                        GameState.update
                            (GameState.Sync content)
                            model.game
                            mode
                            flags
                in
                    ( { model | game = newGame }, cmd )

            "hover" ->
                case decodeHoverOutcome content of
                    Ok hoverOutcome ->
                        let
                            ( newGame, cmd ) =
                                GameState.update
                                    (GameState.HoverOutcome hoverOutcome)
                                    model.game
                                    mode
                                    flags
                        in
                            ( { model | game = newGame }
                            , Cmd.batch
                                [ cmd
                                , playSound "/sfx/hover.wav"
                                ]
                            )

                    Err err ->
                        Debug.log
                            err
                            ( model, Cmd.none )

            "res" ->
                let
                    ( newGame, cmd ) =
                        GameState.update
                            (GameState.ResolveOutcome content)
                            model.game
                            mode
                            flags
                in
                    ( { model | game = newGame }, cmd )

            "syncPlayers" ->
                let
                    newPlayers : Result String ( Maybe String, Maybe String )
                    newPlayers =
                        decodePlayers content
                in
                    case newPlayers of
                        Ok p ->
                            ( { model | players = p }, Cmd.none )

                        Err err ->
                            Debug.log
                                err
                                ( model, Cmd.none )

            "playCard" ->
                let
                    ( newGame, _ ) =
                        GameState.update (GameState.Shake 1.0) model.game mode flags
                in
                    ( { model | game = newGame }, playSound "/sfx/playCard.wav" )

            "end" ->
                ( model, playSound "/sfx/endTurn.wav" )

            otherwise ->
                Debug.log
                    ("Error decoding message from server: " ++ msg)
                    ( model, Cmd.none )
