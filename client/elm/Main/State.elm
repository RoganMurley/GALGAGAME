module Main.State exposing (..)

import Compass.State as Compass
import Compass.Types as Compass
import Mouse
import WebSocket
import Connected.State as Connected
import Drag.Messages as Drag
import Lobby.State as Lobby
import Lobby.Types as Lobby
import Lab.State as Lab
import Menu.Messages as Menu
import Main.Messages exposing (Msg(..))
import Random
import Random.Char exposing (char)
import Random.String exposing (string)
import Tuple exposing (first)
import Util exposing (message, send)
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


update : Msg -> Main.Model -> ( Main.Model, Cmd Msg )
update msg ({ hostname, room, frameTime, seed } as model) =
    case msg of
        UrlChange l ->
            ( locationUpdate model l, Cmd.none )

        Frame dt ->
            ( { model
                | frameTime = frameTime + dt
                , room = tickRoom room dt
              }
            , case room of
                Connected connected ->
                    listen frameTime connected.game

                otherwise ->
                    Cmd.none
            )

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

                                            Lobby.QuickplayGame ->
                                                "quickplay"
                                in
                                    ( model, newUrl <| "/play/" ++ url )

                            otherwise ->
                                ( model, Cmd.none )

                Connecting ({ roomID, gameType } as lobby) ->
                    case msg of
                        StartGame mode ->
                            ( { model | room = Connected <| Connected.init mode roomID }
                            , case gameType of
                                Lobby.ComputerGame ->
                                    Cmd.none

                                Lobby.CustomGame ->
                                    newUrl <| "/play/custom/" ++ roomID

                                Lobby.QuickplayGame ->
                                    Cmd.none
                            )

                        otherwise ->
                            let
                                ( newRoom, cmd ) =
                                    connectingUpdate hostname msg lobby
                            in
                                ( { model | room = Connecting newRoom }, cmd )

                Connected connected ->
                    let
                        ( newRoom, cmd ) =
                            Connected.update hostname msg connected
                    in
                        ( { model | room = Connected newRoom }, cmd )

                Lab lab ->
                    case msg of
                        LabMsg labMsg ->
                            ( { model | room = Lab <| Lab.update lab labMsg }, Cmd.none )

                        otherwise ->
                            ( model, Cmd.none )


connectingUpdate : String -> Msg -> Lobby.Model -> ( Lobby.Model, Cmd Msg )
connectingUpdate hostname msg model =
    case msg of
        LobbyMsg lobbyMsg ->
            Lobby.update model lobbyMsg

        Receive str ->
            ( model, Lobby.receive str )

        otherwise ->
            ( model, Cmd.none )


locationUpdate : Main.Model -> Navigation.Location -> Main.Model
locationUpdate model location =
    case parsePath Compass.route location of
        Just route ->
            case route of
                Compass.Home ->
                    { model | room = initRoom }

                Compass.Lab ->
                    { model | room = Lab Lab.init }

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

                            Compass.QuickPlay ->
                                { model
                                    | room =
                                        Connecting <|
                                            Lobby.modelInit
                                                randomRoomID
                                                Lobby.QuickplayGame
                                }

        Nothing ->
            { model | room = initRoom }


tickRoom : RoomModel -> Float -> RoomModel
tickRoom room dt =
    case room of
        MainMenu ->
            MainMenu

        Connecting model ->
            Connecting model

        Connected model ->
            Connected <| Connected.tick model dt

        Lab model ->
            Lab <| Lab.tick model dt


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
        , AnimationFrame.diffs Frame
        , Window.resizes (\{ width, height } -> Resize width height)
        ]
