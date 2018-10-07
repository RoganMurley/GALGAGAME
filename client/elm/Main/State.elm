module Main.State exposing (..)

import Audio exposing (setVolume)
import Json.Decode as Json
import Http
import Routing.State as Routing
import Routing.Types as Routing
import WebSocket
import GameState.Messages as GameState
import GameState.Types exposing (GameState(Started))
import Connected.Messages as Connected
import Lobby.State as Lobby
import Lobby.Types as Lobby
import Login.Decoders as Login
import Login.State as Login
import Main.Messages exposing (Msg(..))
import Mode exposing (Mode(..))
import Mouse
import Navigation
import Replay.State as Replay
import Room.Messages as Room
import Room.State as Room
import Room.Types as Room
import Room.Generators exposing (generate)
import Util exposing (authLocation, send, websocketAddress)
import Ports exposing (analytics, copyInput, reload, selectAllInput)
import AnimationFrame
import Window
import Listener exposing (listen)
import Main.Types as Main exposing (Flags)
import UrlParser exposing (parsePath)
import Settings.State as Settings
import Settings.Types as Settings
import Texture.State as Texture


init : Flags -> Navigation.Location -> ( Main.Model, Cmd Msg )
init flags location =
    let
        fetchTextures : Cmd Msg
        fetchTextures =
            Cmd.map TextureMsg Texture.fetchTextures

        ( model, cmd ) =
            locationUpdate
                { room = Room.init
                , flags = flags
                , settings = Settings.init
                , textures = Texture.init
                }
                location
    in
        ( model, Cmd.batch [ fetchTextures, cmd ] )


update : Msg -> Main.Model -> ( Main.Model, Cmd Msg )
update msg ({ room, settings, textures, flags } as model) =
    case msg of
        CopyInput elementId ->
            ( model, copyInput elementId )

        Frame dt ->
            ( let
                newFlags =
                    { flags | time = flags.time + dt }
              in
                { model
                    | flags = newFlags
                    , room = Room.tick newFlags room dt
                }
            , case room of
                Room.Connected connected ->
                    listen connected.game

                Room.Replay { replay } ->
                    case replay of
                        Nothing ->
                            Cmd.none

                        Just { state } ->
                            listen (Started state)

                _ ->
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

        UrlChange location ->
            let
                ( newModel, cmd ) =
                    locationUpdate model location
            in
                ( newModel
                , Cmd.batch
                    [ cmd
                    , analytics ()

                    -- , send flags "reconnect:" -- Reopen ws connection
                    ]
                )

        SetVolume volumeType volume ->
            let
                newVolume =
                    clamp 0 100 volume

                newSettings =
                    case volumeType of
                        Settings.Master ->
                            { settings | masterVolume = newVolume }

                        Settings.Music ->
                            { settings | musicVolume = newVolume }

                        Settings.Sfx ->
                            { settings | sfxVolume = newVolume }
            in
                ( { model | settings = newSettings }
                , setVolume newVolume
                )

        Logout ->
            ( model
            , Http.send LogoutCallback <|
                Http.post
                    (authLocation flags ++ "/logout")
                    Http.emptyBody
                    (Json.succeed ())
            )

        LogoutCallback (Ok _) ->
            ( model, reload () )

        LogoutCallback (Err _) ->
            ( model, Cmd.none )

        MousePosition mouse ->
            let
                ( newRoom, newCmd ) =
                    Room.update
                        room
                        (Room.ConnectedMsg <| Connected.GameStateMsg <| GameState.Mouse mouse)
                        flags
            in
                ( { model | room = newRoom }
                , newCmd
                )

        MouseClick mouse ->
            let
                ( newRoom, newCmd ) =
                    Room.update
                        room
                        (Room.ConnectedMsg <| Connected.GameStateMsg <| GameState.MouseClick mouse)
                        flags
            in
                ( { model | room = newRoom }
                , newCmd
                )

        GetAuth ->
            ( model
            , Http.send GetAuthCallback <|
                Http.get
                    (authLocation flags ++ "/me")
                    (Json.maybe Login.authDecoder)
            )

        GetAuthCallback (Ok username) ->
            let
                newFlags : Flags
                newFlags =
                    { flags | username = username }
            in
                ( { model | flags = newFlags }
                , Lobby.skipLobbyCmd username
                )

        GetAuthCallback (Err _) ->
            ( model, Cmd.none )

        TextureMsg textureMsg ->
            let
                ( newTextures, cmd ) =
                    Texture.update textureMsg textures
            in
                ( { model | textures = newTextures }
                , Cmd.map TextureMsg cmd
                )


locationUpdate : Main.Model -> Navigation.Location -> ( Main.Model, Cmd Msg )
locationUpdate model location =
    case parsePath Routing.route location of
        Just route ->
            case route of
                Routing.Home ->
                    ( { model | room = Room.init }
                    , Cmd.none
                    )

                Routing.Play playRoute ->
                    let
                        username : Maybe String
                        username =
                            model.flags.username

                        randomRoomID : String
                        randomRoomID =
                            generate Room.Generators.roomID model.flags.seed
                    in
                        case playRoute of
                            Routing.ComputerPlay ->
                                ( { model
                                    | room =
                                        Room.Lobby <|
                                            Lobby.init
                                                randomRoomID
                                                Lobby.ComputerGame
                                                Playing
                                  }
                                , Lobby.skipLobbyCmd username
                                )

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
                                            ( model, Lobby.skipLobbyCmd username )

                                        _ ->
                                            ( lobbyModel, Lobby.skipLobbyCmd username )

                            Routing.QuickPlay ->
                                ( { model
                                    | room =
                                        Room.Lobby <|
                                            Lobby.init
                                                randomRoomID
                                                Lobby.QuickplayGame
                                                Playing
                                  }
                                , Lobby.skipLobbyCmd username
                                )

                Routing.Spec roomID ->
                    ( { model
                        | room =
                            Room.Lobby <|
                                Lobby.init
                                    roomID
                                    Lobby.ComputerGame
                                    Spectating
                      }
                    , Cmd.none
                    )

                Routing.Replay replayID ->
                    ( { model
                        | room =
                            Room.Replay Replay.init
                      }
                    , Replay.getReplay replayID
                    )

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

                                _ ->
                                    Nothing
                    in
                        ( { model
                            | room =
                                Room.Login <|
                                    Login.init nextPath
                          }
                        , Cmd.none
                        )

        Nothing ->
            ( { model | room = Room.init }
            , Cmd.none
            )


subscriptions : Main.Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen (websocketAddress model.flags) Receive
        , AnimationFrame.diffs Frame
        , Window.resizes (\{ width, height } -> Resize width height)
        , Mouse.moves MousePosition
        , Mouse.clicks MouseClick
        ]
