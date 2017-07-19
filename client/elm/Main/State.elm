module Main.State exposing (..)

import Audio exposing (SoundOption(..), playSound, playSoundWith)
import Compass.State as Compass
import Compass.Types as Compass
import Keyboard
import Mouse
import String exposing (dropLeft, length, startsWith)
import WebSocket
import Chat.Messages as Chat
import Chat.State as Chat
import Drag.Messages as Drag
import Drag.State as Drag exposing (dragAt, dragEnd, dragStart, getPosition)
import Settings.State as Settings
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
import Ports exposing (copyInput, selectAllInput)
import AnimationFrame
import Window
import Listener exposing (listen)
import Main.Types as Main exposing (..)
import Navigation as Navigation exposing (newUrl)
import UrlParser exposing (parsePath)


initModel : Flags -> Navigation.Location -> Main.Model
initModel ({ hostname, httpPort, seed, windowDimensions } as flags) location =
    locationUpdate
        { room = initRoom
        , hostname = hostname
        , httpPort = httpPort
        , frameTime = 0
        , windowDimensions = windowDimensions
        , seed = seed
        }
        location


initRoom : RoomModel
initRoom =
    MainMenu


initConnected : Mode -> String -> ConnectedModel
initConnected mode roomID =
    { chat = Chat.init
    , game = Waiting
    , settings = Settings.init
    , mode = mode
    , roomID = roomID
    }


update : Msg -> Main.Model -> ( Main.Model, Cmd Msg )
update msg ({ hostname, room, frameTime, seed } as model) =
    case msg of
        UrlChange l ->
            ( locationUpdate model l, Cmd.none )

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
                MainMenu ->
                    let
                        roomID : String
                        roomID =
                            generate roomIDGenerator seed
                    in
                        case msg of
                            MenuMsg (Menu.Start gameType) ->
                                let
                                    url : String
                                    url =
                                        case gameType of
                                            Lobby.ComputerGame ->
                                                "computer"

                                            Lobby.CustomGame ->
                                                "custom"
                                in
                                    ( model, newUrl <| "/play/" ++ url )

                            otherwise ->
                                ( model, Cmd.none )

                Connecting ({ roomID, gameType } as lobby) ->
                    case msg of
                        StartGame mode ->
                            ( { model | room = Connected <| initConnected mode roomID }
                            , case gameType of
                                Lobby.ComputerGame ->
                                    Cmd.none

                                Lobby.CustomGame ->
                                    newUrl <| "/play/custom/" ++ roomID
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
connectedUpdate hostname msg ({ chat, game, settings, mode } as model) =
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
                    , playSound "/sfx/endTurn.wav"
                    ]
                )
            )

        PlayCard index ->
            ( model
            , turnOnly model
                (Cmd.batch
                    [ send hostname ("play:" ++ (toString index))
                    , playSound "/sfx/playCard.wav"
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

        SettingsMsg settingsMsg ->
            let
                newsettings =
                    Settings.update settingsMsg settings
            in
                ( { model | settings = newsettings }, Cmd.none )

        -- Enter key
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

        Concede ->
            ( model, send hostname "concede:" )

        otherwise ->
            Debug.crash "Unexpected action while connected ;_;"


locationUpdate : Main.Model -> Navigation.Location -> Main.Model
locationUpdate model location =
    case parsePath Compass.route location of
        Just route ->
            case route of
                Compass.Home ->
                    { model | room = initRoom }

                Compass.Play playRoute ->
                    let
                        randomRoomID : String
                        randomRoomID =
                            generate roomIDGenerator model.seed
                    in
                        case playRoute of
                            Compass.ComputerPlay ->
                                { model
                                    | room =
                                        Connecting <|
                                            Lobby.modelInit
                                                randomRoomID
                                                Lobby.ComputerGame
                                }

                            Compass.CustomPlay mRoomID ->
                                let
                                    roomID : String
                                    roomID =
                                        case mRoomID of
                                            Just r ->
                                                r

                                            Nothing ->
                                                randomRoomID

                                    lobbyModel : Main.Model
                                    lobbyModel =
                                        { model
                                            | room =
                                                Connecting <|
                                                    Lobby.modelInit
                                                        roomID
                                                        Lobby.CustomGame
                                        }
                                in
                                    case model.room of
                                        -- Annoying stateful bit, fix me.
                                        -- WILL cause bugs.
                                        Connected _ ->
                                            model

                                        otherwise ->
                                            lobbyModel

        Nothing ->
            { model | room = initRoom }


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
            , playSound "/sfx/hover.wav"
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
        ( model, playSound "/sfx/playCard.wav" )
    else if (startsWith "end:" msg) then
        ( model, playSound "/sfx/endTurn.wav" )
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


generate : Random.Generator a -> Seed -> a
generate generator seed =
    first <| Random.step generator <| Random.initialSeed seed


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
