module Connected.State exposing (init, mouseDown, mouseUp, receive, tick, update)

import Assets.Types as Assets
import Audio.State exposing (playSound)
import Connected.Decoders exposing (decodeDamageOutcome, decodePlayers)
import Connected.Messages exposing (Msg(..))
import Connected.Types exposing (Model, Players)
import GameState.Messages as GameState
import GameState.State as GameState
import GameState.Types exposing (GameState(..))
import GameType exposing (GameType)
import Hover exposing (decodeHoverOther)
import Json.Decode as Json
import Main.Messages as Main
import Main.Types exposing (Flags)
import Mode exposing (Mode(..))
import Mouse
import PlayState.Messages as PlayState
import Ports exposing (log, websocketSend)
import Settings.Messages as Settings
import Stats exposing (decodeStatChange)
import Util exposing (message, splitOnColon)
import Waiting.State as Waiting
import Waiting.Types exposing (WaitType(..))


init : Mode -> GameType -> String -> Model
init mode gameType roomID =
    { game = Waiting <| Waiting.init WaitQuickplay
    , gameType = gameType
    , mode = mode
    , roomID = roomID
    , players = { pa = Nothing, pb = Nothing }
    , tick = 0
    , errored = False
    }


update : Flags -> Assets.Model -> Msg -> Model -> ( Model, Cmd Main.Msg )
update flags assets msg ({ game, mode, gameType } as model) =
    case msg of
        GameStateMsg gameMsg ->
            let
                ( newGame, cmd ) =
                    GameState.update gameMsg game flags mode gameType assets
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
            GameState.tick flags model.game model.gameType dt

        newTick =
            model.tick + dt
    in
    ( { model | game = game, tick = newTick }, Cmd.map GameStateMsg msg )


receive : Flags -> Assets.Model -> Model -> String -> ( Model, Cmd Main.Msg )
receive flags assets ({ mode, gameType } as model) msg =
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
                        flags
                        mode
                        gameType
                        assets
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
                                flags
                                mode
                                gameType
                                assets
                    in
                    ( { model | game = newGame }
                    , Cmd.batch
                        [ cmd
                        , playSound assets.audio "/sfx/hover.mp3"
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
                                flags
                                mode
                                gameType
                                assets
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
                        flags
                        mode
                        gameType
                        assets
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
                        flags
                        mode
                        gameType
                        assets
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
                                flags
                                mode
                                gameType
                                assets
                    in
                    ( { model | game = newGame }, cmd )

                Err err ->
                    ( model, log <| Json.errorToString err )

        _ ->
            ( { model | errored = False }, log <| "Error decoding message from server: " ++ msg )


mouseUp : Flags -> Assets.Model -> Model -> Mouse.Position -> ( Model, Cmd Main.Msg )
mouseUp flags assets model pos =
    let
        ( game, cmd ) =
            GameState.mouseUp pos model.game flags model.mode model.gameType assets
    in
    ( { model | game = game }, cmd )


mouseDown : Flags -> Assets.Model -> Model -> Mouse.Position -> ( Model, Cmd Main.Msg )
mouseDown flags assets model pos =
    let
        ( game, cmd ) =
            GameState.mouseDown pos model.game flags model.mode model.gameType assets
    in
    ( { model | game = game }, cmd )
