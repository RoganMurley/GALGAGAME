module Main.State exposing (init, locationUpdate, subscriptions, update)

import Assets.Messages as Assets
import Assets.State as Assets
import Audio.State exposing (setVolume)
import Browser
import Browser.Events
import Browser.Navigation
import Connected.State as Connected
import Feedback.State as Feedback
import GameState.Types exposing (GameState(..))
import GameType
import Http
import Json.Decode as Json
import Keyboard
import Listener exposing (listen)
import Lobby.State as Lobby
import Login.Decoders as Login
import Login.State as Login
import Main.Messages as Main exposing (Msg(..))
import Main.Types as Main exposing (Flags)
import Manifest.State as Manifest
import Math.Vector2 exposing (vec2)
import Mode exposing (Mode(..))
import Mouse exposing (MouseState(..))
import Notifications.State as Notifications
import Ports exposing (analytics, copyInput, godModeCommand, mouseDown, mouseMove, mouseUp, reload, selectAllInput, touch, websocketListen, websocketReconnect, websocketSend)
import Replay.State as Replay
import Room.Generators exposing (generate)
import Room.Messages as Room
import Room.State as Room
import Room.Types as Room
import Routing.State as Routing
import Routing.Types as Routing
import Settings.State as Settings
import Settings.Types as Settings
import Signup.State as Signup
import Url exposing (Url)
import Url.Parser exposing (parse)
import Util exposing (authLocation)


init : Flags -> Url -> Int -> ( Main.Model, Cmd Msg )
init flags url initialVolume =
    let
        fetchManifest =
            List.map (Cmd.map (AssetsMsg << Assets.ManifestMsg)) Manifest.fetch

        ( model, cmd ) =
            locationUpdate
                { room = Room.init
                , flags = flags
                , settings = Settings.init initialVolume
                , assets = Assets.init
                , notifications = Notifications.init
                }
                url
    in
    ( model
    , Cmd.batch (cmd :: fetchManifest)
    )


update : Msg -> Main.Model -> ( Main.Model, Cmd Msg )
update msg ({ assets, room, notifications, settings, flags } as model) =
    case msg of
        CopyInput elementId ->
            ( model, copyInput elementId )

        Frame rawDt ->
            let
                dt =
                    settings.gameSpeed * min 30 rawDt

                newFlags =
                    { flags | time = flags.time + dt }

                ( newRoom, tickMsg ) =
                    Room.tick newFlags room dt

                newMsg =
                    case room of
                        Room.Connected { game, tick } ->
                            listen assets.audio game tick

                        Room.Replay { replay } ->
                            case replay of
                                Nothing ->
                                    Cmd.none

                                Just { state, tick } ->
                                    listen assets.audio (Started state) tick

                        _ ->
                            Cmd.none
            in
            ( { model | flags = newFlags, room = newRoom }
            , Cmd.batch [ newMsg, Cmd.map RoomMsg tickMsg ]
            )

        KeyPress key ->
            let
                ( newModel, cmd ) =
                    case model.room of
                        Room.Connected connected ->
                            ( model, Connected.keyPress connected key )

                        Room.Login _ ->
                            ( model, Login.keyPress key )

                        Room.Signup _ ->
                            ( model, Signup.keyPress key )

                        _ ->
                            ( model, Cmd.none )
            in
            ( newModel, cmd )

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
            ( model, websocketSend str )

        SettingsMsg settingsMsg ->
            ( { model | settings = Settings.update settingsMsg settings }
            , Cmd.none
            )

        Receive str ->
            let
                ( newRoom, cmd ) =
                    Room.receive flags assets str room

                newNotifications =
                    Notifications.receive str notifications
            in
            ( { model | room = newRoom, notifications = newNotifications }, cmd )

        RoomMsg roomMsg ->
            let
                ( newRoom, cmd ) =
                    Room.update room roomMsg assets flags
            in
            ( { model | room = newRoom }, cmd )

        NotificationsMsg notificationsMsg ->
            ( { model
                | notifications =
                    Notifications.update model.notifications notificationsMsg
              }
            , Cmd.none
            )

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    update (UrlChange url) model

                Browser.External url ->
                    ( model, Browser.Navigation.load url )

        UrlChange location ->
            let
                ( newModel, cmd ) =
                    locationUpdate model location
            in
            ( newModel
            , Cmd.batch [ cmd, analytics () ]
            )

        SetScaling scaling ->
            let
                newScaling =
                    clamp 0.25 2 scaling

                newFlags =
                    { flags | scaling = newScaling }
            in
            ( { model | flags = newFlags }
            , Ports.scaling newScaling
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

        MousePosition pos ->
            let
                newFlags =
                    { flags
                        | mouse = Mouse <| vec2 (toFloat pos.x) (toFloat pos.y)
                    }

                newCmd =
                    Cmd.none
            in
            ( { model | flags = newFlags }
            , newCmd
            )

        MouseDown mouse ->
            let
                ( newRoom, newCmd ) =
                    Room.mouseDown
                        flags
                        assets
                        room
                        mouse
            in
            ( { model | room = newRoom }
            , newCmd
            )

        MouseUp mouse ->
            let
                ( newRoom, newCmd ) =
                    Room.mouseUp
                        flags
                        assets
                        room
                        mouse
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

        TouchPosition position ->
            let
                mouse =
                    case position of
                        Just { x, y } ->
                            Touch <| vec2 (toFloat x) (toFloat y)

                        Nothing ->
                            NoMouse

                newFlags =
                    { flags | mouse = mouse }

                ( newRoom, newCmd ) =
                    case ( flags.mouse, position ) of
                        ( NoMouse, Just pos ) ->
                            Room.mouseDown
                                newFlags
                                assets
                                room
                                pos

                        ( Touch vec, Nothing ) ->
                            Room.mouseUp
                                newFlags
                                assets
                                room
                                { x = round <| Math.Vector2.getX vec
                                , y = round <| Math.Vector2.getY vec
                                }

                        _ ->
                            ( room, Cmd.none )
            in
            ( { model | flags = newFlags, room = newRoom }
            , newCmd
            )

        GodCommand str ->
            ( model, websocketSend <| "god:" ++ str )

        AssetsMsg assetsMsg ->
            let
                ( newAssets, cmd ) =
                    Assets.update assetsMsg assets
            in
            ( { model | assets = newAssets }, cmd )

        NoOp ->
            ( model, Cmd.none )


locationUpdate : Main.Model -> Url -> ( Main.Model, Cmd Msg )
locationUpdate model url =
    let
        nextPath : Maybe String
        nextPath =
            case model.room of
                Room.Lobby { gameType, roomID } ->
                    case gameType of
                        GameType.CustomGame ->
                            Just <| "/play/custom/" ++ roomID

                        GameType.ComputerGame ->
                            Just "/play/computer"

                        GameType.QuickplayGame ->
                            Just "/play/quickplay"

                _ ->
                    Nothing

        route =
            parse Routing.route url |> Maybe.withDefault Routing.default
    in
    case route of
        Routing.Home ->
            ( { model | room = Room.init }
            , websocketReconnect ()
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
                                    GameType.ComputerGame
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
                                            GameType.CustomGame
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
                                    GameType.QuickplayGame
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
                            GameType.ComputerGame
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
            ( { model
                | room =
                    Room.Login <|
                        Login.init nextPath
              }
            , Cmd.none
            )

        Routing.Signup ->
            ( { model
                | room =
                    Room.Signup <|
                        Signup.init nextPath
              }
            , Cmd.none
            )

        Routing.Feedback ->
            ( { model
                | room =
                    Room.Feedback <|
                        Feedback.init nextPath
              }
            , Cmd.none
            )


subscriptions : Main.Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ websocketListen Receive
        , Browser.Events.onAnimationFrameDelta Frame
        , Browser.Events.onResize Resize
        , mouseMove MousePosition
        , mouseDown MouseDown
        , mouseUp MouseUp
        , touch TouchPosition
        , Browser.Events.onKeyPress (Json.map KeyPress Keyboard.keyDecoder)
        , godModeCommand GodCommand
        ]
