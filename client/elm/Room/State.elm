module Room.State exposing (init, receive, tick, update)

import Browser.Navigation
import Connected.State as Connected
import GameType
import Lobby.State as Lobby
import Login.State as Login
import Main.Messages as Main
import Main.Types exposing (Flags)
import Menu.State as Menu
import Replay.State as Replay
import Room.Messages exposing (Msg(..))
import Room.Types exposing (Model(..))
import Signup.State as Signup


init : Model
init =
    MainMenu


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg flags =
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
                            Connected.update connectedMsg connected
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
                    ( Replay <| Replay.update replay replayMsg, Cmd.none )

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

        StartGame mode ->
            case model of
                Lobby { gameType, roomID } ->
                    ( Connected <| Connected.init mode gameType roomID
                    , case gameType of
                        GameType.ComputerGame ->
                            Cmd.none

                        GameType.CustomGame ->
                            Browser.Navigation.pushUrl flags.key <|
                                "/play/custom/"
                                    ++ roomID

                        GameType.QuickplayGame ->
                            Cmd.none

                        GameType.TutorialGame ->
                            Cmd.none

                        GameType.DailyGame ->
                            Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


receive : String -> Model -> ( Model, Cmd Main.Msg )
receive str model =
    case model of
        MainMenu ->
            ( MainMenu, Cmd.none )

        Lobby lobby ->
            ( Lobby lobby, Lobby.receive str )

        Connected connected ->
            let
                ( newConnected, cmd ) =
                    Connected.receive connected str
            in
            ( Connected newConnected, cmd )

        Replay replay ->
            ( Replay replay, Replay.receive str )

        Login login ->
            ( Login login, Login.receive str )

        Signup signup ->
            ( Signup signup, Signup.receive str )


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
