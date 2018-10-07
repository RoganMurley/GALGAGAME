module Room.State exposing (init, tick, receive, update)

import Connected.State as Connected
import Lobby.State as Lobby
import Lobby.Types as Lobby
import Login.State as Login
import Main.Messages as Main
import Main.Types exposing (Flags)
import Menu.State as Menu
import Navigation exposing (newUrl)
import Room.Types exposing (Model(..))
import Room.Messages exposing (Msg(..))
import Replay.State as Replay


init : Model
init =
    MainMenu


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        MenuMsg menuMsg ->
            case model of
                MainMenu ->
                    ( model, Menu.update menuMsg )

                _ ->
                    ( model, Cmd.none )

        LobbyMsg lobbyMsg ->
            case model of
                Lobby lobby ->
                    let
                        ( newLobby, newMsg ) =
                            Lobby.update lobby lobbyMsg
                    in
                        ( Lobby newLobby, newMsg )

                _ ->
                    ( model, Cmd.none )

        ConnectedMsg connectedMsg ->
            case model of
                Connected connected ->
                    let
                        ( newConnected, cmd ) =
                            Connected.update flags connectedMsg connected
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

        StartGame mode ->
            case model of
                Lobby { gameType, roomID } ->
                    ( Connected <| Connected.init mode roomID
                    , case gameType of
                        Lobby.ComputerGame ->
                            Cmd.none

                        Lobby.CustomGame ->
                            newUrl <| "/play/custom/" ++ roomID

                        Lobby.QuickplayGame ->
                            Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


receive : String -> Model -> Flags -> ( Model, Cmd Main.Msg )
receive str model flags =
    case model of
        MainMenu ->
            ( MainMenu, Cmd.none )

        Lobby lobby ->
            ( Lobby lobby, Lobby.receive str )

        Connected connected ->
            let
                ( newConnected, cmd ) =
                    Connected.receive connected str flags
            in
                ( Connected newConnected, cmd )

        Replay replay ->
            ( Replay replay, Replay.receive str )

        Login login ->
            ( Login login, Login.receive str )


tick : Flags -> Model -> Float -> Model
tick flags room dt =
    case room of
        MainMenu ->
            MainMenu

        Lobby lobby ->
            Lobby lobby

        Login login ->
            Login login

        Connected connected ->
            Connected <| Connected.tick flags connected dt

        Replay replay ->
            Replay <| Replay.tick flags replay dt
