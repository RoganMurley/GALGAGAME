module Connected.State exposing (init, keyPress, mouseDown, mouseUp, receive, tick, update)

import Assets.Types as Assets
import Audio.State exposing (playSound)
import Chat.Messages as Chat
import Chat.State as Chat
import Connected.Decoders exposing (decodeDamageOutcome)
import Connected.Messages exposing (Msg(..))
import Connected.Types exposing (Model)
import GameState.Messages as GameState
import GameState.State as GameState
import GameState.Types exposing (GameState(..))
import GameType exposing (GameType)
import Hover exposing (decodeHoverOther)
import Json.Decode as Json
import Keyboard exposing (Key(..))
import Main.Messages as Main
import Main.Types exposing (Flags)
import Math.Vector2 exposing (vec2)
import Mode exposing (Mode(..))
import Mouse
import PlayState.Messages as PlayState
import Players exposing (Players)
import Ports exposing (log, websocketSend)
import Ripple.State as Ripple
import Settings.Messages as Settings
import Stats exposing (decodeStatChange)
import Util exposing (message, splitOnColon)
import Waiting.State as Waiting
import Waiting.Types exposing (WaitType(..))


heartbeatInterval : Float
heartbeatInterval =
    10 * 1000


init : Mode -> GameType -> String -> Model
init mode gameType roomID =
    { game = Waiting <| Waiting.init WaitQuickplay
    , gameType = gameType
    , mode = mode
    , roomID = roomID
    , players = { pa = Nothing, pb = Nothing }
    , tick = 0
    , errored = False
    , chat = Chat.init
    , heartbeatTick = 0
    , connectionLost = False
    , ripples = []
    }


keyPress : Model -> Key -> Cmd Main.Msg
keyPress { chat } keycode =
    Chat.keyPress chat keycode


update : Flags -> Assets.Model -> Msg -> Model -> ( Model, Cmd Main.Msg )
update flags assets msg ({ chat, game, mode, gameType, players } as model) =
    case msg of
        ChatMsg chatMsg ->
            let
                ( newChat, cmd ) =
                    Chat.update chatMsg chat assets
            in
            ( { model | chat = newChat }, cmd )

        GameStateMsg gameMsg ->
            let
                ( newGame, cmd ) =
                    GameState.update gameMsg game flags mode gameType players assets
            in
            ( { model | game = newGame }, cmd )

        Concede ->
            ( model
            , Cmd.batch
                [ message <|
                    Main.SettingsMsg <|
                        Settings.CloseMenu
                , websocketSend "concede:"
                ]
            )


tick : Flags -> Model -> Float -> ( Model, Cmd Msg )
tick flags model dt =
    if model.connectionLost then
        ( model, Cmd.none )

    else
        let
            chat =
                Chat.tick flags model.chat dt

            ( game, msg ) =
                GameState.tick flags model.game model.chat model.gameType dt

            newTick =
                model.tick + dt

            ( heartbeatTick, heartbeatCmds ) =
                if model.heartbeatTick <= 0 then
                    ( heartbeatInterval, [ Ports.websocketSend "heartbeat:" ] )

                else
                    ( model.heartbeatTick - dt, [] )

            ripples =
                Ripple.tick model.ripples dt

            cmd =
                Cmd.batch (Cmd.map GameStateMsg msg :: heartbeatCmds)
        in
        ( { model
            | chat = chat
            , game = game
            , heartbeatTick = heartbeatTick
            , tick = newTick
            , ripples = ripples
          }
        , cmd
        )


receive : Flags -> Assets.Model -> Model -> String -> ( Model, Cmd Main.Msg )
receive flags assets model msg =
    let
        ( command, content ) =
            splitOnColon msg

        { mode, gameType, players } =
            model
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
                        players
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
                                players
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
                                players
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
                        players
                        assets
            in
            ( { model | game = newGame }, cmd )

        "syncPlayers" ->
            let
                newPlayers : Result Json.Error Players
                newPlayers =
                    Players.decode content
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
                        players
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
                                players
                                assets
                    in
                    ( { model | game = newGame }, cmd )

                Err err ->
                    ( model, log <| Json.errorToString err )

        "chat" ->
            let
                ( newChat, cmd ) =
                    Chat.update (Chat.RecvMessage content) model.chat assets
            in
            ( { model | chat = newChat }, cmd )

        "timeLeft" ->
            case String.toFloat content of
                Just timeLeft ->
                    let
                        ( game, cmd ) =
                            GameState.update
                                (GameState.PlayStateMsg <|
                                    PlayState.ServerTimeLeft <|
                                        timeLeft
                                )
                                model.game
                                flags
                                mode
                                gameType
                                players
                                assets

                        -- Attempt to align the heartbeat with timeLeft
                        heartbeatTick =
                            min model.heartbeatTick timeLeft
                    in
                    ( { model | game = game, heartbeatTick = heartbeatTick }, cmd )

                Nothing ->
                    ( model, log <| "Unable to parse timeLeft value: " ++ content )

        "connectionLost" ->
            ( { model | connectionLost = True }, Cmd.none )

        _ ->
            ( { model | errored = False }, log <| "Error decoding message from server: " ++ msg )


mouseUp : Flags -> Assets.Model -> Model -> Mouse.Position -> ( Model, Cmd Main.Msg )
mouseUp flags assets model pos =
    let
        chat =
            Chat.mouseUp flags model.chat

        ( game, cmd ) =
            GameState.mouseUp pos model.game flags model.mode model.gameType assets
    in
    ( { model | chat = chat, game = game }, cmd )


mouseDown : Flags -> Assets.Model -> Model -> Mouse.Position -> ( Model, Cmd Main.Msg )
mouseDown flags assets model pos =
    let
        ( game, cmd ) =
            GameState.mouseDown pos model.game flags model.mode model.gameType model.players assets

        ripples =
            Ripple.add
                (vec2 (toFloat pos.x) (toFloat pos.y))
                model.ripples
    in
    ( { model | game = game, ripples = ripples }, cmd )
