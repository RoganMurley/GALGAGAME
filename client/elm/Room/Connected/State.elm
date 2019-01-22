module Connected.State exposing (init, receive, tick, update)

import Audio exposing (playSound)
import Connected.Decoders exposing (decodeHoverOutcome, decodePlayers)
import Connected.Messages exposing (Msg(..))
import Connected.Types exposing (Model)
import GameState.Messages as GameState
import GameState.State as GameState
import GameState.Types exposing (GameState(..), WaitType(..))
import Main.Messages as Main
import Main.Types exposing (Flags)
import Mode exposing (Mode(..))
import PlayState.Messages as PlayState
import Settings.Messages as Settings
import Util exposing (message, send, splitOnColon)


init : Mode -> String -> Model
init mode roomID =
    { game = Waiting WaitQuickplay
    , mode = mode
    , roomID = roomID
    , players = ( Nothing, Nothing )
    }


update : Flags -> Msg -> Model -> ( Model, Cmd Main.Msg )
update flags msg ({ game, mode } as model) =
    case msg of
        GameStateMsg gameMsg ->
            let
                ( newGame, cmd ) =
                    GameState.update gameMsg game mode flags
            in
            ( { model | game = newGame }, cmd )

        Concede ->
            model
                ! [ message <|
                        Main.SettingsMsg <|
                            Settings.CloseSettings
                  , send flags "concede:"
                  ]


tick : Flags -> Model -> Float -> ( Model, Cmd Msg )
tick flags model dt =
    let
        ( game, msg ) =
            GameState.tick flags model.game dt
    in
    ( { model | game = game }, Cmd.map GameStateMsg msg )


receive : Model -> String -> Flags -> ( Model, Cmd Main.Msg )
receive ({ mode } as model) msg flags =
    let
        ( command, content ) =
            splitOnColon msg
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
                                (GameState.PlayStateMsg <|
                                    PlayState.HoverOutcome hoverOutcome
                                )
                                model.game
                                mode
                                flags
                    in
                    { model | game = newGame }
                        ! [ cmd
                          , playSound "/sfx/hover.wav"
                          ]

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

        "replaySaved" ->
            let
                ( newGame, cmd ) =
                    GameState.update
                        (GameState.PlayStateMsg <|
                            PlayState.ReplaySaved content
                        )
                        model.game
                        mode
                        flags
            in
            ( { model | game = newGame }, cmd )

        _ ->
            Debug.log
                ("Error decoding message from server: " ++ msg)
                ( model, Cmd.none )
