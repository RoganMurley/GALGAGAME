module Main.State exposing (..)

import Audio exposing (setVolume)
import Json.Decode as Json
import Http
import Routing.State as Routing
import Routing.Types as Routing
import WebSocket
import Lab.State as Lab
import Lobby.State as Lobby
import Lobby.Types as Lobby
import Login.State as Login
import Main.Messages exposing (Msg(..))
import Mode exposing (Mode(..))
import Navigation
import Room.State as Room
import Room.Types as Room
import Room.Generators exposing (generate)
import Util exposing (authLocation, send, websocketAddress)
import Ports exposing (analytics, copyInput, reload, selectAllInput)
import AnimationFrame
import Window
import Listener exposing (listen)
import Main.Types as Main exposing (..)
import UrlParser exposing (parsePath)
import Settings.State as Settings


init : Flags -> Navigation.Location -> Main.Model
init flags location =
    locationUpdate
        { room = Room.init
        , flags = flags
        , settings = Settings.init
        }
        location


update : Msg -> Main.Model -> ( Main.Model, Cmd Msg )
update msg ({ room, settings, flags } as model) =
    let
        { time, seed } =
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
                ( model, send flags str )

            SettingsMsg settingsMsg ->
                ( { model | settings = Settings.update settingsMsg settings }
                , Cmd.none
                )

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
                ( locationUpdate model l
                , Cmd.batch
                    [ analytics ()
                    , send flags "reconnect:" -- Reopen ws connection
                    ]
                )

            SetVolume volume ->
                let
                    newVolume =
                        clamp 0 100 volume
                in
                    ( { model
                        | settings =
                            { settings | volume = newVolume }
                      }
                    , setVolume newVolume
                    )

            Logout ->
                ( model
                , Http.send LogoutCallback <|
                    Http.post
                        ((authLocation flags) ++ "/logout")
                        Http.emptyBody
                        (Json.succeed ())
                )

            LogoutCallback (Ok _) ->
                ( model, reload () )

            LogoutCallback (Err _) ->
                ( model, Cmd.none )


locationUpdate : Main.Model -> Navigation.Location -> Main.Model
locationUpdate model location =
    case parsePath Routing.route location of
        Just route ->
            case route of
                Routing.Home ->
                    { model | room = Room.init }

                Routing.Lab ->
                    { model | room = Room.Lab Lab.init }

                Routing.Play playRoute ->
                    let
                        randomRoomID : String
                        randomRoomID =
                            generate Room.Generators.roomID model.flags.seed
                    in
                        case playRoute of
                            Routing.ComputerPlay ->
                                { model
                                    | room =
                                        Room.Lobby <|
                                            Lobby.init
                                                randomRoomID
                                                Lobby.ComputerGame
                                                Playing
                                }

                            Routing.CustomPlay mRoomID ->
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

                            Routing.QuickPlay ->
                                { model
                                    | room =
                                        Room.Lobby <|
                                            Lobby.init
                                                randomRoomID
                                                Lobby.QuickplayGame
                                                Playing
                                }

                Routing.Spec roomID ->
                    { model
                        | room =
                            Room.Lobby <|
                                Lobby.init
                                    roomID
                                    Lobby.ComputerGame
                                    Spectating
                    }

                Routing.Login ->
                    let
                        -- Annoying stateful bit, fix me.
                        -- WILL cause bugs.
                        nextPath : Maybe String
                        nextPath =
                            case model.room of
                                Room.Lobby { gameType, roomID } ->
                                    case gameType of
                                        Lobby.CustomGame ->
                                            Just <| "/play/custom/" ++ roomID

                                        Lobby.ComputerGame ->
                                            Just "/play/computer/"

                                        Lobby.QuickplayGame ->
                                            Just "/play/quickplay/"

                                otherwise ->
                                    Nothing
                    in
                        { model
                            | room =
                                Room.Login <|
                                    Login.init nextPath
                        }

        Nothing ->
            { model | room = Room.init }


subscriptions : Main.Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen (websocketAddress model.flags) Receive
        , AnimationFrame.diffs Frame
        , Window.resizes (\{ width, height } -> Resize width height)
        ]
