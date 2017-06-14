module Main.State exposing (..)

import Audio exposing (SoundOption(..), playSound, playSoundWith)
import Keyboard
import Mouse
import String exposing (dropLeft, length, startsWith)
import WebSocket
import Chat.Messages as Chat
import Chat.State as Chat
import Drag.Messages as Drag
import Drag.State as Drag exposing (dragAt, dragEnd, dragStart, getPosition)
import GameState.Messages as GameState
import GameState.Types as GameState exposing (GameState(..))
import GameState.State as GameState exposing (resTick, tickForward, tickZero)
import Lobby.State as Lobby
import Lobby.Types as Lobby
import Menu.Messages as Menu
import Model.Types exposing (Hand, Model, WhichPlayer(..))
import Main.Messages exposing (Msg(..))
import Random
import Random.Char exposing (char)
import Random.String exposing (string)
import Time exposing (Time, second)
import Tuple exposing (first)
import Util exposing (message)
import Ports exposing (copyInput, selectAllInput, queryParams)
import AnimationFrame
import Window
import Listener exposing (listen)
import Main.Types as Main exposing (..)


initModel : Flags -> Main.Model
initModel ({ hostname, httpPort, play, seed, windowDimensions } as flags) =
    { room =
        case play of
            Just roomID ->
                Connecting <|
                    Lobby.modelInit roomID Lobby.CustomGame

            Nothing ->
                MainMenu seed
    , hostname = hostname
    , httpPort = httpPort
    , frameTime = 0
    , windowDimensions = windowDimensions
    }


update : Msg -> Main.Model -> ( Main.Model, Cmd Msg )
update msg ({ hostname, room, frameTime } as model) =
    case msg of
        Frame dt ->
            ( { model | frameTime = frameTime + dt }, Cmd.none )

        Resize w h ->
            ( { model | windowDimensions = ( w, h ) }, Cmd.none )

        SelectAllInput elementId ->
            ( model, selectAllInput elementId )

        CopyInput elementId ->
            ( model, copyInput elementId )

        Send str ->
            ( model, send hostname str )

        otherwise ->
            case room of
                MainMenu seed ->
                    let
                        generate : Random.Generator a -> a
                        generate generator =
                            first <| Random.step generator <| Random.initialSeed seed

                        roomID : String
                        roomID =
                            generate roomIDGenerator
                    in
                        case msg of
                            MenuMsg (Menu.Start gameType) ->
                                ( { model | room = Connecting <| Lobby.modelInit roomID gameType }, Cmd.none )

                            otherwise ->
                                ( model, Cmd.none )

                Connecting ({ roomID } as lobby) ->
                    case msg of
                        StartGame mode ->
                            ( { model
                                | room =
                                    Connected
                                        { chat = Chat.init
                                        , game = Waiting
                                        , mode = mode
                                        , roomID = roomID
                                        }
                              }
                            , queryParams <| "?play=" ++ roomID
                            )

                        otherwise ->
                            let
                                ( newRoom, cmd ) =
                                    connectingUpdate hostname msg lobby
                            in
                                ( { model | room = Connecting newRoom }, cmd )

                Connected connectedModel ->
                    let
                        ( newRoom, cmd ) =
                            connectedUpdate hostname msg connectedModel
                    in
                        ( { model | room = Connected newRoom }, cmd )


connectingUpdate : String -> Msg -> Lobby.Model -> ( Lobby.Model, Cmd Msg )
connectingUpdate hostname msg model =
    case msg of
        LobbyMsg lobbyMsg ->
            Lobby.update model lobbyMsg

        Receive str ->
            ( model, Lobby.receive str )

        otherwise ->
            ( model, Cmd.none )


connectedUpdate : String -> Msg -> ConnectedModel -> ( ConnectedModel, Cmd Msg )
connectedUpdate hostname msg ({ chat, game, mode } as model) =
    case msg of
        Receive str ->
            connectedReceive model str

        DragMsg dragMsg ->
            ( { model | chat = Drag.update dragMsg chat }, Cmd.none )

        DrawCard ->
            ( model, turnOnly model (send hostname "draw:") )

        EndTurn ->
            ( model
            , turnOnly model
                (Cmd.batch
                    [ send hostname "end:"
                    , playSound "sfx/endTurn.wav"
                    ]
                )
            )

        PlayCard index ->
            ( model
            , turnOnly model
                (Cmd.batch
                    [ send hostname ("play:" ++ (toString index))
                    , playSound "sfx/playCard.wav"
                    ]
                )
            )

        ChatMsg chatMsg ->
            let
                ( newChat, cmd ) =
                    Chat.update chat chatMsg
            in
                ( { model | chat = newChat }, cmd )

        GameStateMsg gameMsg ->
            let
                ( newGame, cmd ) =
                    GameState.update gameMsg game
            in
                ( { model | game = newGame }, cmd )

        KeyPress 13 ->
            let
                ( chat, cmd ) =
                    Chat.update model.chat Chat.Send
            in
                ( { model | chat = chat }, cmd )

        KeyPress _ ->
            ( model, Cmd.none )

        Tick t ->
            let
                tickedGame =
                    tickForward game

                resolveCommand =
                    if tickZero model.game then
                        [ message ResolveStep ]
                    else
                        []

                listenCommand =
                    [ listen t tickedGame ]

                commands =
                    resolveCommand ++ listenCommand
            in
                ( { model | game = tickedGame }
                , Cmd.batch commands
                )

        ResolveStep ->
            ( { model | game = resTick game }, Cmd.none )

        Rematch ->
            case model.game of
                Ended which _ _ ->
                    ( model, playingOnly model <| send hostname "rematch:" )

                otherwise ->
                    ( model, Cmd.none )

        HoverCard name ->
            let
                cardName =
                    case name of
                        Just x ->
                            toString x

                        Nothing ->
                            "null"
            in
                ( model
                , Cmd.batch
                    [ playingOnly model <| message <| Send <| "hover:" ++ cardName
                    , case name of
                        Nothing ->
                            Cmd.none

                        otherwise ->
                            playSound "sfx/hover.wav"
                    ]
                )

        PlayingOnly newMsg ->
            ( model, playingOnly model <| message newMsg )

        otherwise ->
            Debug.crash "Unexpected action while connected ;_;"


connectedReceive : ConnectedModel -> String -> ( ConnectedModel, Cmd Msg )
connectedReceive model msg =
    if startsWith "chat:" msg then
        ( model
        , message <|
            ChatMsg <|
                Chat.New <|
                    dropLeft (length "chat:") msg
        )
    else if (startsWith "sync:" msg) then
        ( model
        , message <|
            GameStateMsg <|
                GameState.Sync <|
                    dropLeft (length "sync:") msg
        )
    else if (startsWith "hover:" msg) then
        ( model
        , Cmd.batch
            [ message <|
                GameStateMsg <|
                    GameState.HoverOutcome <|
                        parseHoverOutcome <|
                            dropLeft (length "hover:") msg
            , playSound "sfx/hover.wav"
            ]
        )
    else if (startsWith "res:" msg) then
        ( model
        , message <|
            GameStateMsg <|
                GameState.ResolveOutcome <|
                    dropLeft (length "res:") msg
        )
    else if (startsWith "playCard:" msg) then
        ( model, playSound "sfx/playCard.wav" )
    else if (startsWith "end:" msg) then
        ( model, playSound "sfx/endTurn.wav" )
    else
        Debug.crash ("Error decoding message from server: " ++ msg)


parseHoverOutcome : String -> Maybe Int
parseHoverOutcome msg =
    case msg of
        "null" ->
            Nothing

        otherwise ->
            case String.toInt msg of
                Ok index ->
                    Just index

                Err err ->
                    Debug.crash err


send : String -> String -> Cmd Msg
send hostname =
    WebSocket.send <| "ws://" ++ hostname ++ ":9160"


playingOnly : ConnectedModel -> Cmd Msg -> Cmd Msg
playingOnly { mode } cmdMsg =
    case mode of
        Spectating ->
            Cmd.none

        Playing ->
            cmdMsg


turnOnly : ConnectedModel -> Cmd Msg -> Cmd Msg
turnOnly { mode, game } cmdMsg =
    case mode of
        Spectating ->
            Cmd.none

        Playing ->
            case game of
                PlayingGame model res ->
                    case model.turn of
                        PlayerA ->
                            cmdMsg

                        PlayerB ->
                            Cmd.none

                otherwise ->
                    Cmd.none


roomIDGenerator : Random.Generator String
roomIDGenerator =
    string 8 Random.Char.english


usernameNumberGenerator : Random.Generator String
usernameNumberGenerator =
    string 3 <| char 48 57


validateName : String -> ( Bool, String )
validateName name =
    if length name > 20 then
        ( False, "username too long" )
    else if String.isEmpty name then
        ( False, "" )
    else
        ( True, "" )


subscriptions : Main.Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen ("ws://" ++ model.hostname ++ ":9160") Receive
        , Mouse.moves <| DragMsg << Drag.At
        , Mouse.ups <| DragMsg << Drag.End
        , Keyboard.presses KeyPress
        , Time.every (second / 60) Tick
        , AnimationFrame.diffs Frame
        , Window.resizes (\{ width, height } -> Resize width height)
        ]
