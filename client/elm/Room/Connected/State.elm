module Connected.State exposing (init, receive, tick, update)

import Audio.State exposing (playSound)
import Connected.Decoders exposing (decodeDamageOutcome, decodePlayers)
import Connected.Messages exposing (Msg(..))
import Connected.Types exposing (Model, Players)
import Game.Decoders exposing (decodeHoverOther)
import GameState.Messages as GameState
import GameState.State as GameState
import GameState.Types exposing (GameState(..), WaitType(..))
import GameType exposing (GameType)
import Json.Decode as Json
import Main.Messages as Main
import Main.Types exposing (Flags)
import Mode exposing (Mode(..))
import PlayState.Messages as PlayState
import Ports exposing (log, websocketSend)
import Settings.Messages as Settings
import Stats exposing (decodeStatChange)
import Util exposing (message, splitOnColon)


init : Mode -> GameType -> String -> Model
init mode gameType roomID =
    { game = Waiting WaitQuickplay
    , gameType = gameType
    , mode = mode
    , roomID = roomID
    , players = { pa = Nothing, pb = Nothing }
    , tick = 0
    }


update : Msg -> Model -> ( Model, Cmd Main.Msg )
update msg ({ game, mode } as model) =
    case msg of
        GameStateMsg gameMsg ->
            let
                ( newGame, cmd ) =
                    GameState.update gameMsg game mode
            in
            ( { model | game = newGame }, cmd )

        Concede ->
            ( model
            , Cmd.batch
                [ message <|
                    Main.SettingsMsg <|
                        Settings.CloseSettings
                , websocketSend "concede:"
                ]
            )


tick : Flags -> Model -> Float -> ( Model, Cmd Msg )
tick flags model dt =
    let
        ( game, msg ) =
            GameState.tick flags model.game dt

        newTick =
            model.tick + dt
    in
    ( { model | game = game, tick = newTick }, Cmd.map GameStateMsg msg )


receive : Model -> String -> ( Model, Cmd Main.Msg )
receive ({ mode } as model) msg =
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
                    in
                    ( { model | game = newGame }
                    , Cmd.batch
                        [ cmd
                        , playSound "/sfx/hover.mp3"
                        ]
                    )

                Err err ->
                    ( model, log <| Json.errorToString err )

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
                    in
                    ( { model | game = newGame }, cmd )

                Err err ->
                    ( model, log <| Json.errorToString err )

        "res" ->
            let
                ( newGame, cmd ) =
                    GameState.update
                        (GameState.ResolveOutcome content)
                        model.game
                        mode
            in
            ( { model | game = newGame }, cmd )

        "syncPlayers" ->
            let
                newPlayers : Result Json.Error Players
                newPlayers =
                    decodePlayers content
            in
            case newPlayers of
                Ok p ->
                    ( { model | players = p }, Cmd.none )

                Err err ->
                    ( model, log <| Json.errorToString err )

        "replaySaved" ->
            let
                ( newGame, cmd ) =
                    GameState.update
                        (GameState.PlayStateMsg <|
                            PlayState.ReplaySaved content
                        )
                        model.game
                        mode
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
                    in
                    ( { model | game = newGame }, cmd )

                Err err ->
                    ( model, log <| Json.errorToString err )

        _ ->
            ( model, log <| "Error decoding message from server: " ++ msg )
