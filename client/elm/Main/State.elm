module Main.State exposing (init, locationUpdate, subscriptions, update)

import Assets.Messages as Assets
import Assets.State as Assets
import Audio.State exposing (setVolume)
import Browser
import Browser.Events
import Browser.Navigation
import Connected.Messages as Connected
import Feedback.State as Feedback
import GameState.Messages as GameState
import GameState.Types exposing (GameState(..))
import GameType
import Http
import Json.Decode as Json
import Keyboard
import Listener exposing (listen)
import Lobby.State as Lobby
import Login.Decoders as Login
import Login.State as Login
import Main.Messages exposing (Msg(..))
import Main.Types as Main exposing (Flags)
import Manifest.State as Manifest
import Math.Vector2 exposing (vec2)
import Mode exposing (Mode(..))
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
                }
                url
    in
    ( model
    , Cmd.batch (cmd :: fetchManifest)
    )


update : Msg -> Main.Model -> ( Main.Model, Cmd Msg )
update msg ({ assets, room, settings, flags } as model) =
    case msg of
        CopyInput elementId ->
            ( model, copyInput elementId )

        Frame rawDt ->
            let
                dt =
                    min 30 rawDt

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

        KeyPress keyCode ->
            let
                ( newModel, cmd ) =
                    case model.room of
                        Room.Login _ ->
                            ( model, Login.keyPress keyCode )

                        Room.Signup _ ->
                            ( model, Signup.keyPress keyCode )

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
            in
            ( { model | room = newRoom }, cmd )

        RoomMsg roomMsg ->
            let
                ( newRoom, cmd ) =
                    Room.update room roomMsg assets flags
            in
            ( { model | room = newRoom }, cmd )

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
                    { flags | mouse = Just (vec2 (toFloat pos.x) (toFloat pos.y)) }

                newCmd =
                    Cmd.none
            in
            ( { model | flags = newFlags }
            , newCmd
            )

        MouseDown mouse ->
            let
                ( newRoom, newCmd ) =
                    Room.update
                        room
                        (Room.ConnectedMsg <|
                            Connected.GameStateMsg <|
                                GameState.MouseDown mouse
                        )
                        assets
                        flags
            in
            ( { model | room = newRoom }
            , newCmd
            )

        MouseUp mouse ->
            let
                ( newRoom, newCmd ) =
                    Room.update
                        room
                        (Room.ConnectedMsg <|
                            Connected.GameStateMsg <|
                                GameState.MouseUp mouse
                        )
                        assets
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

        TouchPosition pos ->
            let
                -- ( newRoom, newCmd ) =
                --     Room.update
                --         room
                --         (Room.ConnectedMsg <|
                --             Connected.GameStateMsg <|
                --                 GameState.Touch pos
                --         )
                --         flags
                newFlags =
                    { flags | mouse = Maybe.map (\{ x, y } -> vec2 (toFloat x) (toFloat y)) pos }

                newCmd =
                    Cmd.none
            in
            ( { model | flags = newFlags }
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

                        GameType.TutorialGame ->
                            Just "/play/tutorial"

                        GameType.DailyGame ->
                            Just "/play/daily"

                _ ->
                    Nothing
    in
    case parse Routing.route url of
        Just route ->
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

                        Routing.TutorialPlay ->
                            ( { model
                                | room =
                                    Room.Lobby <|
                                        Lobby.init
                                            randomRoomID
                                            GameType.TutorialGame
                                            Playing
                              }
                            , Lobby.skipLobbyCmd username
                            )

                        Routing.DailyPlay ->
                            ( { model
                                | room =
                                    Room.Lobby <|
                                        Lobby.init
                                            randomRoomID
                                            GameType.DailyGame
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

        Nothing ->
            ( { model | room = Room.init }
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
