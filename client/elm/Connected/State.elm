module Connected.State exposing (init, update, tick)

import Audio exposing (playSound, setVolume)
import Connected.Types exposing (..)
import Connected.Decoders exposing (decodeHoverOutcome, decodePlayers)
import GameState.Messages as GameState
import GameState.State as GameState
import GameState.Types exposing (..)
import Model.Types exposing (WhichPlayer(..))
import Settings.State as Settings
import Settings.Messages as Settings
import Main.Messages exposing (Msg(..))
import String exposing (dropLeft, length, startsWith)
import Util exposing (message, send)


init : Mode -> String -> Model
init mode roomID =
    { game = Waiting WaitQuickplay
    , settings = Settings.init
    , mode = mode
    , roomID = roomID
    , players = ( Nothing, Nothing )
    }


update : String -> Msg -> Model -> ( Model, Cmd Msg )
update hostname msg ({ game, settings, mode } as model) =
    case msg of
        Receive str ->
            receive model str

        DragMsg dragMsg ->
            ( model, Cmd.none )

        DrawCard ->
            ( model, turnOnly model (send hostname "draw:") )

        EndTurn ->
            ( model
            , turnOnly model
                (Cmd.batch
                    [ send hostname "end:"
                    , playSound "/sfx/endTurn.wav"
                    ]
                )
            )

        PlayCard index ->
            let
                ( newGame1, cmd1 ) =
                    GameState.update (GameState.HoverSelf Nothing) game

                ( newGame2, cmd2 ) =
                    GameState.update (GameState.Shake 1.0) newGame1
            in
                ( { model | game = newGame2 }
                , turnOnly model
                    (Cmd.batch
                        [ send hostname ("play:" ++ (toString index))
                        , playSound "/sfx/playCard.wav"
                        , cmd1
                        , cmd2
                        ]
                    )
                )

        GameStateMsg gameMsg ->
            let
                ( newGame, cmd ) =
                    GameState.update gameMsg game
            in
                ( { model | game = newGame }, cmd )

        SettingsMsg settingsMsg ->
            let
                newsettings =
                    Settings.update settingsMsg settings
            in
                ( { model | settings = newsettings }, Cmd.none )

        Rematch ->
            case model.game of
                Ended which _ _ _ _ ->
                    ( model, playingOnly model <| send hostname "rematch:" )

                otherwise ->
                    ( model, Cmd.none )

        HoverCard mIndex ->
            let
                index =
                    case mIndex of
                        Just x ->
                            toString x

                        Nothing ->
                            "null"

                ( newGame, cmd ) =
                    GameState.update
                        (GameState.HoverSelf mIndex)
                        model.game
            in
                ( { model | game = newGame }
                , Cmd.batch
                    [ cmd
                    , playingOnly model <| message <| Send <| "hover:" ++ index
                    , case mIndex of
                        Nothing ->
                            Cmd.none

                        otherwise ->
                            playSound "/sfx/hover.wav"
                    ]
                )

        PlayingOnly newMsg ->
            ( model, playingOnly model <| message newMsg )

        Concede ->
            let
                newSettings =
                    Settings.update Settings.CloseSettings settings
            in
                ( { model | settings = newSettings }, send hostname "concede:" )

        SetVolume volume ->
            let
                newVolume =
                    clamp 0 100 volume

                newSettings =
                    { settings | volume = newVolume }
            in
                ( { model | settings = newSettings }
                , setVolume newVolume
                )

        otherwise ->
            Debug.log
                ("Unexpected action while connected ;_;")
                ( model, Cmd.none )


tick : Model -> Float -> Model
tick model dt =
    { model | game = GameState.tick model.game dt }


playingOnly : Model -> Cmd Msg -> Cmd Msg
playingOnly { mode } cmdMsg =
    case mode of
        Spectating ->
            Cmd.none

        Playing ->
            cmdMsg


turnOnly : Model -> Cmd Msg -> Cmd Msg
turnOnly { mode, game } cmdMsg =
    case mode of
        Spectating ->
            Cmd.none

        Playing ->
            case game of
                PlayingGame ( model, vm ) res ->
                    case model.turn of
                        PlayerA ->
                            cmdMsg

                        PlayerB ->
                            Cmd.none

                otherwise ->
                    Cmd.none


receive : Model -> String -> ( Model, Cmd Msg )
receive model msg =
    if (startsWith "sync:" msg) then
        let
            ( newGame, cmd ) =
                GameState.update
                    (GameState.Sync <| dropLeft (length "sync:") msg)
                    model.game
        in
            ( { model | game = newGame }, cmd )
    else if (startsWith "hover:" msg) then
        case decodeHoverOutcome <| dropLeft (length "hover:") msg of
            Ok hoverOutcome ->
                let
                    ( newGame, cmd ) =
                        GameState.update
                            (GameState.HoverOutcome hoverOutcome)
                            model.game
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
    else if (startsWith "res:" msg) then
        let
            ( newGame, cmd ) =
                GameState.update
                    (GameState.ResolveOutcome <| dropLeft (length "res:") msg)
                    model.game
        in
            ( { model | game = newGame }, cmd )
    else if (startsWith "syncPlayers:" msg) then
        let
            newPlayers : Result String ( Maybe String, Maybe String )
            newPlayers =
                decodePlayers (dropLeft (length "syncPlayers:") msg)
        in
            case newPlayers of
                Ok p ->
                    ( { model | players = p }, Cmd.none )

                Err err ->
                    Debug.log
                        err
                        ( model, Cmd.none )
    else if (startsWith "playCard:" msg) then
        let
            ( newGame, _ ) =
                GameState.update (GameState.Shake 1.0) model.game
        in
            ( { model | game = newGame }, playSound "/sfx/playCard.wav" )
    else if (startsWith "end:" msg) then
        ( model, playSound "/sfx/endTurn.wav" )
    else
        Debug.log
            ("Error decoding message from server: " ++ msg)
            ( model, Cmd.none )
