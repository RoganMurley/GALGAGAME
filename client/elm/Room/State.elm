module Room.State exposing (init, mouseDown, mouseUp, receive, tick, update)

import Assets.Types as Assets
import Browser.Navigation
import Connected.State as Connected
import Feedback.State as Feedback
import GameType exposing (GameType(..))
import Lobby.State as Lobby
import Login.State as Login
import Main.Messages as Main
import Main.Types exposing (Flags)
import Menu.State as Menu
import Mouse
import Ports exposing (log)
import Replay.State as Replay
import Room.Messages exposing (Msg(..))
import Room.Types exposing (Model(..))
import Signup.State as Signup
import Util exposing (message)
import World.Messages as World
import World.State as World


init : Model
init =
    MainMenu


update : Model -> Msg -> Assets.Model -> Flags -> ( Model, Cmd Main.Msg )
update model msg assets flags =
    case msg of
        MenuMsg menuMsg ->
            case model of
                MainMenu ->
                    ( model, Menu.update menuMsg flags )

                _ ->
                    ( model, Cmd.none )

        LobbyMsg lobbyMsg ->
            case model of
                Lobby lobby ->
                    let
                        ( newLobby, newMsg ) =
                            Lobby.update lobby lobbyMsg flags
                    in
                    ( Lobby newLobby, newMsg )

                _ ->
                    ( model, Cmd.none )

        ConnectedMsg connectedMsg ->
            case model of
                Connected connected ->
                    let
                        ( newConnected, cmd ) =
                            Connected.update flags assets connectedMsg connected
                    in
                    ( Connected newConnected, cmd )

                _ ->
                    ( model, Cmd.none )

        LoginMsg loginMsg ->
            case model of
                Login login ->
                    let
                        ( newLogin, cmd ) =
                            Login.update login loginMsg flags
                    in
                    ( Login newLogin, cmd )

                _ ->
                    ( model, Cmd.none )

        ReplayMsg replayMsg ->
            case model of
                Replay replay ->
                    let
                        ( newReplay, cmd ) =
                            Replay.update replay replayMsg
                    in
                    ( Replay newReplay, cmd )

                _ ->
                    ( model, Cmd.none )

        SignupMsg signupMsg ->
            case model of
                Signup signup ->
                    let
                        ( newSignup, cmd ) =
                            Signup.update signup signupMsg flags
                    in
                    ( Signup newSignup, cmd )

                _ ->
                    ( model, Cmd.none )

        FeedbackMsg feedbackMsg ->
            case model of
                Feedback feedback ->
                    let
                        ( newFeedback, cmd ) =
                            Feedback.update feedback feedbackMsg flags
                    in
                    ( Feedback newFeedback, cmd )

                _ ->
                    ( model, Cmd.none )

        WorldMsg worldMsg ->
            case model of
                World world ->
                    let
                        ( newWorld, cmd ) =
                            World.update world worldMsg flags
                    in
                    ( World newWorld, cmd )

                _ ->
                    ( model, Cmd.none )

        StartGame mode messageRoomID ->
            case model of
                Lobby lobby ->
                    let
                        roomID =
                            Maybe.withDefault lobby.roomID messageRoomID
                    in
                    ( Connected <| Connected.init mode lobby.gameType roomID
                    , case lobby.gameType of
                        GameType.CustomGame ->
                            Browser.Navigation.pushUrl flags.key <|
                                "/play/custom/"
                                    ++ roomID

                        _ ->
                            Cmd.none
                    )

                World _ ->
                    case messageRoomID of
                        Just roomID ->
                            ( Connected <| Connected.init mode WorldGame roomID
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, log "Missing room ID from World start game" )

                _ ->
                    ( model, Cmd.none )

        VisitWorld endEncounter ->
            ( World World.init
            , if endEncounter then
                message <| Main.Send "endEncounter:"

              else
                Cmd.none
            )


receive : Flags -> Assets.Model -> String -> Model -> ( Model, Cmd Main.Msg )
receive flags assets str model =
    case model of
        MainMenu ->
            ( MainMenu, Cmd.none )

        Lobby lobby ->
            ( Lobby lobby, Lobby.receive str )

        Connected connected ->
            let
                ( newConnected, cmd ) =
                    Connected.receive flags assets connected str
            in
            ( Connected newConnected, cmd )

        Replay replay ->
            ( Replay replay, Replay.receive str )

        Login login ->
            ( Login login, Login.receive str )

        Signup signup ->
            ( Signup signup, Signup.receive str )

        Feedback feedback ->
            ( Feedback feedback, Feedback.receive str )

        World world ->
            ( World world, World.receive str )


tick : Flags -> Model -> Float -> ( Model, Cmd Msg )
tick flags room dt =
    case room of
        MainMenu ->
            ( MainMenu, Cmd.none )

        Lobby lobby ->
            ( Lobby lobby, Cmd.none )

        Login login ->
            ( Login login, Cmd.none )

        Connected connected ->
            let
                ( newConnected, msg ) =
                    Connected.tick flags connected dt
            in
            ( Connected newConnected, Cmd.map ConnectedMsg msg )

        Replay replay ->
            ( Replay <| Replay.tick flags replay dt, Cmd.none )

        Signup signup ->
            ( Signup signup, Cmd.none )

        Feedback feedback ->
            ( Feedback feedback, Cmd.none )

        World world ->
            ( World <| World.tick flags world dt, Cmd.none )


mouseUp : Flags -> Assets.Model -> Model -> Mouse.Position -> ( Model, Cmd Main.Msg )
mouseUp flags assets model pos =
    case model of
        Connected connected ->
            let
                ( newConnected, cmd ) =
                    Connected.mouseUp flags assets connected pos
            in
            ( Connected newConnected, cmd )

        World world ->
            let
                ( newWorld, cmd ) =
                    World.mouseUp flags assets world pos
            in
            ( World newWorld, cmd )

        _ ->
            ( model, Cmd.none )


mouseDown : Flags -> Assets.Model -> Model -> Mouse.Position -> ( Model, Cmd Main.Msg )
mouseDown flags assets model pos =
    case model of
        Connected connected ->
            let
                ( newConnected, cmd ) =
                    Connected.mouseDown flags assets connected pos
            in
            ( Connected newConnected, cmd )

        World world ->
            let
                ( newWorld, cmd ) =
                    World.mouseDown flags assets world pos
            in
            ( World newWorld, cmd )

        _ ->
            ( model, Cmd.none )
