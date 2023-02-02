module Main.State exposing (init, locationUpdate, subscriptions, update)

import Assets.Messages as Assets
import Assets.State as Assets
import Audio.State exposing (setVolume)
import Browser
import Browser.Events exposing (Visibility(..))
import Browser.Navigation
import Connected.Messages as Connected
import Connected.State as Connected
import Create.State as Create
import DeckBuilding.Messages as DeckBuilding
import Feedback.State as Feedback
import GameState.Messages as GameState
import GameState.Types exposing (GameState(..))
import GameType
import Http
import Json.Decode as Json
import Keyboard
import Leaderboard.Messages as Leaderboard
import Leaderboard.State as Leaderboard
import League.Messages as League
import League.State as League
import Listener exposing (listen)
import Lobby.State as Lobby
import Login.Decoders as Login
import Login.State as Login
import Main.Messages as Main exposing (Msg(..))
import Main.Types as Main exposing (Flags)
import Main.View exposing (titleView)
import Manifest.State as Manifest
import Math.Vector2 exposing (vec2)
import Mode exposing (Mode(..))
import Mouse exposing (MouseState(..))
import Notifications.State as Notifications
import Ports exposing (analytics, copyInput, godModeCommand, loadSavedCharacter, log, mouseDown, mouseMove, mouseUp, reload, selectAllInput, touch, websocketListen, websocketSend)
import Profile.Messages as Profile
import Profile.State as Profile
import Replay.Messages as Replay
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
import Util exposing (apiLocation, message)


init : Flags -> Url -> Int -> Int -> ( Main.Model, Cmd Msg )
init flags url initialVolume initialMusicVolume =
    let
        fetchManifest =
            List.map (Cmd.map (AssetsMsg << Assets.ManifestMsg)) Manifest.fetch

        ( model, cmd ) =
            locationUpdate
                { room = Room.init
                , flags = flags
                , settings = Settings.init initialVolume initialMusicVolume
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

        Reload ->
            ( model, Browser.Navigation.reload )

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
                    let
                        ( newModel, cmd ) =
                            update (UrlChange url) model
                    in
                    ( newModel
                    , Cmd.batch
                        [ Browser.Navigation.pushUrl flags.key <| Url.toString url
                        , cmd
                        ]
                    )

                Browser.External url ->
                    ( model, Browser.Navigation.load url )

        UrlChange location ->
            let
                ( newModel, cmd ) =
                    locationUpdate model location
            in
            ( newModel
            , Cmd.batch
                [ cmd
                , analytics ()
                ]
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

        SetUsername username ->
            let
                newFlags =
                    { flags | username = Just username }
            in
            ( { model | flags = newFlags }, Cmd.none )

        SetVolume volumeType volume ->
            let
                newVolume =
                    clamp 0 100 volume

                newSettings =
                    case volumeType of
                        Settings.Music ->
                            { settings | musicVolume = newVolume }

                        Settings.Sfx ->
                            { settings | sfxVolume = newVolume }
            in
            ( { model | settings = newSettings }
            , setVolume volumeType newVolume
            )

        Logout ->
            ( model
            , Http.post
                { url =
                    apiLocation flags ++ "/logout"
                , body =
                    Http.emptyBody
                , expect = Http.expectWhatever LogoutCallback
                }
            )

        LogoutCallback (Ok _) ->
            ( model, reload () )

        LogoutCallback (Err _) ->
            ( model, log "Error logging in" )

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

        VisibilityChange visibility ->
            let
                newRoom =
                    Room.visibilityChange room visibility

                newFlags =
                    { flags
                        | visibility = visibility
                    }

                -- Reset the title in case it was change in the background.
                cmd =
                    case visibility of
                        Visible ->
                            Ports.setTitle (titleView model)

                        Hidden ->
                            Cmd.none
            in
            ( { model | room = newRoom, flags = newFlags }, cmd )

        GotoLogin ->
            ( { model | settings = Settings.close settings }
            , Browser.Navigation.pushUrl flags.key "/login"
            )

        GotoSignup ->
            ( { model | settings = Settings.close settings }
            , Browser.Navigation.pushUrl flags.key "/signup"
            )

        GotoCustomGame ->
            ( { model | settings = Settings.close settings }
            , Browser.Navigation.load "/play/custom"
            )

        NoOp ->
            ( model, Cmd.none )


locationUpdate : Main.Model -> Url -> ( Main.Model, Cmd Msg )
locationUpdate model url =
    let
        nextPath : Maybe String
        nextPath =
            case model.room of
                Room.Lobby { gameType, mode, roomID } ->
                    case mode of
                        Playing ->
                            case gameType of
                                GameType.CustomGame ->
                                    Just <| "/play/custom/" ++ roomID

                                GameType.ComputerGame ->
                                    Just <| "/play/computer/" ++ roomID

                                GameType.QuickplayGame ->
                                    Just <| "/play/quickplay/" ++ roomID

                        Spectating ->
                            Just <| "/spec/" ++ roomID

                _ ->
                    Nothing

        route =
            parse Routing.route url
                |> Maybe.withDefault Routing.default
    in
    case route of
        Routing.Play playRoute ->
            let
                username : Maybe String
                username =
                    model.flags.username

                randomRoomID : String
                randomRoomID =
                    generate Room.Generators.roomID model.flags.seed

                makeModel : Maybe String -> GameType.GameType -> Main.Model
                makeModel mRoomID gameType =
                    let
                        roomID : String
                        roomID =
                            case mRoomID of
                                Just r ->
                                    r

                                Nothing ->
                                    randomRoomID

                        joinAttempts : Int
                        joinAttempts =
                            case model.room of
                                Room.Lobby lobby ->
                                    lobby.joinAttempts

                                _ ->
                                    0
                    in
                    case model.room of
                        Room.Connected _ ->
                            model

                        _ ->
                            { model
                                | room =
                                    Room.Lobby <|
                                        Lobby.init
                                            roomID
                                            joinAttempts
                                            gameType
                                            Playing
                            }

                newModel : Main.Model
                newModel =
                    case playRoute of
                        Routing.ComputerPlay mRoomID ->
                            makeModel mRoomID GameType.ComputerGame

                        Routing.CustomPlay mRoomID ->
                            makeModel mRoomID GameType.CustomGame

                        Routing.QuickPlay mRoomID ->
                            makeModel mRoomID GameType.QuickplayGame
            in
            ( newModel, Lobby.skipLobbyCmd username )

        Routing.Spec roomID ->
            ( { model
                | room =
                    Room.Lobby <|
                        Lobby.init
                            roomID
                            0
                            GameType.CustomGame
                            Spectating
              }
            , Lobby.skipLobbyCmd model.flags.username
            )

        Routing.Replay replayId rawFrame ->
            let
                frame =
                    toFloat <| Maybe.withDefault 0 rawFrame
            in
            case model.room of
                Room.Replay _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | room =
                            Room.Replay <| Replay.init replayId frame
                      }
                    , message <| Main.RoomMsg <| Room.ReplayMsg <| Replay.Load replayId frame
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

        Routing.League ->
            ( { model
                | room =
                    Room.League <|
                        League.init
              }
            , message <| Main.RoomMsg <| Room.LeagueMsg <| League.CheckState
            )

        Routing.Leaderboard ->
            ( { model
                | room =
                    Room.Leaderboard <|
                        Leaderboard.init
              }
            , message <| Main.RoomMsg <| Room.LeaderboardMsg <| Leaderboard.Load
            )

        Routing.Profile username ->
            ( { model
                | room =
                    Room.Profile <|
                        Profile.init
              }
            , message <| Main.RoomMsg <| Room.ProfileMsg <| Profile.Load username
            )

        Routing.Create ->
            ( { model
                | room =
                    Room.Create <|
                        Create.init
              }
            , Cmd.none
            )


subscriptions : Main.Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ websocketListen Receive
        , Browser.Events.onAnimationFrameDelta Frame
        , Browser.Events.onResize Resize
        , Browser.Events.onVisibilityChange VisibilityChange
        , mouseMove MousePosition
        , mouseDown MouseDown
        , mouseUp MouseUp
        , touch TouchPosition
        , Browser.Events.onKeyPress (Json.map KeyPress Keyboard.keyDecoder)
        , godModeCommand GodCommand
        , loadSavedCharacter
            (RoomMsg
                << Room.ConnectedMsg
                << Connected.GameStateMsg
                << GameState.SelectingMsg
                << DeckBuilding.LoadSavedCharacter
            )
        ]
