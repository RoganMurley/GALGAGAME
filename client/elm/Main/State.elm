module Main.State exposing (..)

import Compass.State as Compass
import Compass.Types as Compass
import WebSocket
import Lab.State as Lab
import Lobby.State as Lobby
import Lobby.Types as Lobby
import Main.Messages exposing (Msg(..))
import Mode exposing (Mode(..))
import Navigation
import Room.State as Room
import Room.Types as Room
import Room.Generators exposing (generate)
import Util exposing (message, send)
import Ports exposing (copyInput, selectAllInput)
import AnimationFrame
import Window
import Listener exposing (listen)
import Main.Types as Main exposing (..)
import UrlParser exposing (parsePath)


init : Flags -> Navigation.Location -> Main.Model
init flags location =
    locationUpdate
        { room = Room.init
        , flags = flags
        }
        location


update : Msg -> Main.Model -> ( Main.Model, Cmd Msg )
update msg ({ room, flags } as model) =
    let
        { hostname, time, seed } =
            flags
    in
        case msg of
            CopyInput elementId ->
                ( model, copyInput elementId )

            Frame dt ->
                ( { model
                    | flags = { flags | time = time + dt }
                    , room = Room.tick room dt
                  }
                , case room of
                    Room.Connected connected ->
                        listen time connected.game

                    otherwise ->
                        Cmd.none
                )

            Resize w h ->
                ( { model
                    | flags =
                        { flags | dimensions = ( w, h ) }
                  }
                , Cmd.none
                )

            SelectAllInput elementId ->
                ( model, selectAllInput elementId )

            Send str ->
                ( model, send hostname str )

            Receive str ->
                let
                    ( newRoom, cmd ) =
                        Room.receive str room flags
                in
                    ( { model | room = newRoom }, cmd )

            RoomMsg roomMsg ->
                let
                    ( newRoom, cmd ) =
                        Room.update room roomMsg flags
                in
                    ( { model | room = newRoom }, cmd )

            UrlChange l ->
                ( locationUpdate model l, Cmd.none )


locationUpdate : Main.Model -> Navigation.Location -> Main.Model
locationUpdate model location =
    case parsePath Compass.route location of
        Just route ->
            case route of
                Compass.Home ->
                    { model | room = Room.init }

                Compass.Lab ->
                    { model | room = Room.Lab Lab.init }

                Compass.Play playRoute ->
                    let
                        randomRoomID : String
                        randomRoomID =
                            generate Room.Generators.roomID model.flags.seed
                    in
                        case playRoute of
                            Compass.ComputerPlay ->
                                { model
                                    | room =
                                        Room.Lobby <|
                                            Lobby.init
                                                randomRoomID
                                                Lobby.ComputerGame
                                                Playing
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
                                                Room.Lobby <|
                                                    Lobby.init
                                                        roomID
                                                        Lobby.CustomGame
                                                        Playing
                                        }
                                in
                                    case model.room of
                                        -- Annoying stateful bit, fix me.
                                        -- WILL cause bugs.
                                        Room.Connected _ ->
                                            model

                                        otherwise ->
                                            lobbyModel

                            Compass.QuickPlay ->
                                { model
                                    | room =
                                        Room.Lobby <|
                                            Lobby.init
                                                randomRoomID
                                                Lobby.QuickplayGame
                                                Playing
                                }

                Compass.Spec roomID ->
                    { model
                        | room =
                            Room.Lobby <|
                                Lobby.init
                                    roomID
                                    Lobby.ComputerGame
                                    Spectating
                    }

        Nothing ->
            { model | room = Room.init }


subscriptions : Main.Model -> Sub Msg
subscriptions model =
    let
        websocketAddress =
            "ws://" ++ model.flags.hostname ++ ":9160"
    in
        Sub.batch
            [ WebSocket.listen websocketAddress Receive
            , AnimationFrame.diffs Frame
            , Window.resizes (\{ width, height } -> Resize width height)
            ]
