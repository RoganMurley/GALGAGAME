module Main.State exposing (..)

import Audio exposing (SoundOption(..), playSound, playSoundWith)
import Keyboard
import Mouse
import String exposing (dropLeft, length, startsWith)
import WebSocket
import Chat.State as Chat
import Drag exposing (dragAt, dragEnd, dragStart, getPosition)
import GameState.Messages as GameState
import GameState.Types as GameState exposing (GameState(..))
import GameState.State as GameState exposing (resTick, tickForward, tickZero)
import Model.Types exposing (Hand, Model, WhichPlayer(..))
import Main.Messages exposing (MenuMsg(..), Msg(..))
import Random
import Random.Char exposing (char)
import Random.String exposing (string)
import Time exposing (Time, second)
import Tuple exposing (first)
import Util exposing (applyFst, message)
import Ports exposing (copyInput, selectAllInput, queryParams)
import AnimationFrame
import Window
import Listener exposing (listen)
import Main.Types as Main exposing (..)


init : Flags -> ( Main.Model, Cmd Msg )
init ({ hostname, httpPort, play, seed, windowDimensions } as flags) =
    let
        model : Main.Model
        model =
            { room =
                case play of
                    Just roomID ->
                        connectingInit
                            ("player" ++ (first (Random.step usernameNumberGenerator (Random.initialSeed seed))))
                            roomID
                            CustomGame

                    Nothing ->
                        MainMenu seed
            , hostname = hostname
            , httpPort = httpPort
            , frameTime = 0
            , windowDimensions = windowDimensions
            }
    in
        ( model, Cmd.none )


connectingInit : String -> String -> GameType -> RoomModel
connectingInit username roomID gameType =
    Connecting
        { roomID = roomID
        , name = username
        , error = ""
        , valid = True
        , gameType = gameType
        }


update : Msg -> Main.Model -> ( Main.Model, Cmd Msg )
update msg ({ hostname, room, frameTime } as model) =
    case msg of
        Frame dt ->
            ( { model | frameTime = frameTime + dt }, Cmd.none )

        Resize w h ->
            ( { model | windowDimensions = ( w, h ) }, Cmd.none )

        otherwise ->
            case room of
                MainMenu seed ->
                    case msg of
                        MainMenuMsg m ->
                            case m of
                                MenuCustom ->
                                    ( { model
                                        | room =
                                            (connectingInit
                                                ("player" ++ (first (Random.step usernameNumberGenerator (Random.initialSeed seed))))
                                                (first (Random.step roomIDGenerator (Random.initialSeed seed)))
                                                CustomGame
                                            )
                                      }
                                    , Cmd.none
                                    )

                                MenuComputer ->
                                    ( { model
                                        | room =
                                            (connectingInit
                                                ("player" ++ (first (Random.step usernameNumberGenerator (Random.initialSeed seed))))
                                                (first (Random.step roomIDGenerator (Random.initialSeed seed)))
                                                ComputerGame
                                            )
                                      }
                                    , Cmd.none
                                    )

                        otherwise ->
                            ( model, Cmd.none )

                Connecting ({ roomID } as connectingModel) ->
                    case msg of
                        Play ->
                            ( { model | room = Connected { chat = Chat.init, game = Waiting, mode = Playing, roomID = roomID } }, queryParams ("?play=" ++ roomID) )

                        Spectate ->
                            ( { model | room = Connected { chat = Chat.init, game = Waiting, mode = Spectating, roomID = roomID } }, queryParams ("?play=" ++ roomID) )

                        otherwise ->
                            applyFst (\c -> { model | room = Connecting c }) (connectingUpdate hostname msg connectingModel)

                Connected connectedModel ->
                    applyFst (\c -> { model | room = Connected c }) (connectedUpdate hostname msg connectedModel)


connectingUpdate : String -> Msg -> Main.ConnectingModel -> ( Main.ConnectingModel, Cmd Msg )
connectingUpdate hostname msg ({ roomID, name, error, valid, gameType } as model) =
    case msg of
        Input input ->
            ( { model | name = input, error = Tuple.second (validateName input), valid = Tuple.first (validateName input) }, Cmd.none )

        Send str ->
            ( model, Cmd.batch [ send hostname (Debug.log "sending" str), send hostname (Debug.log "sending" ("room:" ++ roomID)) ] )

        Receive str ->
            connectingReceive model str

        DragAt pos ->
            ( model, Cmd.none )

        DragEnd pos ->
            ( model, Cmd.none )

        ConnectError str ->
            ( { model | error = str }, Cmd.none )

        KeyPress 13 ->
            let
                playPrefix : String
                playPrefix =
                    case gameType of
                        CustomGame ->
                            "play:"

                        ComputerGame ->
                            "playComputer:"
            in
                case Tuple.first (validateName name) of
                    False ->
                        ( model, Cmd.none )

                    True ->
                        ( { model | name = "" }, message (Send (playPrefix ++ name)) )

        KeyPress _ ->
            ( model, Cmd.none )

        Tick t ->
            ( model, Cmd.none )

        SelectAllInput elementId ->
            ( model, selectAllInput elementId )

        CopyInput elementId ->
            ( model, copyInput elementId )

        otherwise ->
            Debug.crash "Unexpected action while not connected ;_;"


connectedUpdate : String -> Msg -> ConnectedModel -> ( ConnectedModel, Cmd Msg )
connectedUpdate hostname msg ({ chat, game, mode } as model) =
    case msg of
        Input input ->
            ( { model | chat = { chat | input = input } }, Cmd.none )

        Send str ->
            ( { model | chat = { chat | input = "" } }
            , if str /= "chat:" then
                send hostname str
              else
                Cmd.none
            )

        Receive str ->
            connectedReceive model str

        DragStart pos ->
            ( { model | chat = dragStart chat pos }, Cmd.none )

        DragAt pos ->
            ( { model | chat = dragAt chat pos }, Cmd.none )

        DragEnd pos ->
            ( { model | chat = dragEnd chat }, Cmd.none )

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

        NewChatMsg str ->
            ( { model | chat = Chat.addMessage str chat }, Cmd.none )

        GameStateMsg gameMsg ->
            ( { model | game = GameState.update gameMsg game }, Cmd.none )

        KeyPress 13 ->
            ( model, message (Send ("chat:" ++ chat.input)) )

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
                    ( model, playingOnly model (send hostname "rematch:") )

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
                    [ playingOnly model (message (Send ("hover:" ++ cardName)))
                    , if name /= Nothing then
                        playSound "sfx/hover.wav"
                      else
                        Cmd.none
                    ]
                )

        SelectCharacter name ->
            ( model
            , playingOnly model
                (Cmd.batch
                    [ message (Send ("selectCharacter:" ++ name))
                    , playSound "sfx/endTurn.wav"
                    ]
                )
            )

        SelectAllInput elementId ->
            ( model, selectAllInput elementId )

        CopyInput elementId ->
            ( model, copyInput elementId )

        otherwise ->
            Debug.crash "Unexpected action while connected ;_;"


connectedReceive : ConnectedModel -> String -> ( ConnectedModel, Cmd Msg )
connectedReceive model msg =
    if (startsWith "chat:" msg) then
        ( model, message (NewChatMsg (dropLeft (length "chat:") msg)) )
    else if (startsWith "sync:" msg) then
        ( model
        , message
            (GameStateMsg
                (GameState.Sync
                    (dropLeft (length "sync:") msg)
                )
            )
        )
    else if (startsWith "hover:" msg) then
        ( model
        , Cmd.batch
            [ message
                (GameStateMsg
                    (GameState.HoverOutcome
                        (parseHoverOutcome (dropLeft (length "hover:") msg))
                    )
                )
            , playSound "sfx/hover.wav"
            ]
        )
    else if (startsWith "res:" msg) then
        ( model
        , message
            (GameStateMsg
                (GameState.ResolveOutcome
                    (dropLeft (length "res:") msg)
                )
            )
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


connectingReceive : ConnectingModel -> String -> ( ConnectingModel, Cmd Msg )
connectingReceive model msg =
    if (startsWith "acceptPlay:" msg) then
        ( model, message Play )
    else if (startsWith "acceptSpec:" msg) then
        ( model, message Spectate )
    else if (startsWith "error:" msg) then
        ( model, message (ConnectError (dropLeft (length "error:") msg)) )
    else
        -- Defer other messages.
        ( model, message (Receive msg) )


send : String -> String -> Cmd Msg
send hostname =
    WebSocket.send ("ws://" ++ hostname ++ ":9160")


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
    string 3 (char 48 57)


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
        , Mouse.moves DragAt
        , Mouse.ups DragEnd
        , Keyboard.presses KeyPress
        , Time.every (second / 60) Tick
        , AnimationFrame.diffs Frame
        , Window.resizes (\{ width, height } -> Resize width height)
        ]
