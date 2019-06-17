module Connected.State exposing (init, receive, tick, update)

import Audio exposing (playSound)
import Connected.Decoders exposing (decodeDamageOutcome, decodePlayers)
import Connected.Messages exposing (Msg(..))
import Connected.Types exposing (Model)
import Game.Decoders exposing (decodeHoverOther)
import GameState.Messages as GameState
import GameState.State as GameState
import GameState.Types exposing (GameState(..), WaitType(..))
import GameType exposing (GameType)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Mode exposing (Mode(..))
import PlayState.Messages as PlayState
import Settings.Messages as Settings
import Stats exposing (decodeStatChange)
import Util exposing (message, send, splitOnColon)


init : Mode -> GameType -> String -> Model
init mode gameType roomID =
    { game = Waiting WaitQuickplay
    , gameType = gameType
    , mode = mode
    , roomID = roomID
    , players = ( Nothing, Nothing )
    , tick = 0
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

        newTick =
            model.tick + dt
    in
    ( { model | game = game, tick = newTick }, Cmd.map GameStateMsg msg )


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
            case decodeHoverOther content of
                Ok hoverOther ->
                    let
                        ( newGame, cmd ) =
                            GameState.update
                                (GameState.PlayStateMsg <|
                                    PlayState.HoverOtherOutcome hoverOther
                                )
                                model.game
                                mode
                                flags
                    in
                    { model | game = newGame }
                        ! [ cmd
                          , playSound "/sfx/hover.mp3"
                          ]

                Err err ->
                    Debug.log
                        err
                        ( model, Cmd.none )

        "damage" ->
            case decodeDamageOutcome content of
                Ok damageOutcome ->
                    let
                        ( newGame, cmd ) =
                            GameState.update
                                (GameState.PlayStateMsg <|
                                    PlayState.DamageOutcome damageOutcome
                                )
                                model.game
                                mode
                                flags
                    in
                    ( { model | game = newGame }, cmd )

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

        "xp" ->
            case decodeStatChange content of
                Ok statChange ->
                    let
                        ( newGame, cmd ) =
                            GameState.update
                                (GameState.PlayStateMsg <|
                                    PlayState.StatChange statChange
                                )
                                model.game
                                mode
                                flags
                    in
                    ( { model | game = newGame }, cmd )

                Err err ->
                    Debug.log
                        err
                        ( model, Cmd.none )

        _ ->
            Debug.log
                ("Error decoding message from server: " ++ msg)
                ( model, Cmd.none )
