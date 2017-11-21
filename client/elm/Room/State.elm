module Room.State exposing (init, tick, receive, update)

import Connected.State as Connected
import Lab.State as Lab
import Lobby.State as Lobby
import Lobby.Types as Lobby
import Main.Messages as Main
import Main.Types exposing (Flags)
import Menu.State as Menu
import Navigation exposing (newUrl)
import Room.Types exposing (..)
import Room.Messages exposing (..)


init : Model
init =
    MainMenu


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg ({ hostname, seed } as flags) =
    case msg of
        MenuMsg menuMsg ->
            case model of
                MainMenu ->
                    ( model, Menu.update menuMsg flags )

                otherwise ->
                    ( model, Cmd.none )

        LobbyMsg lobbyMsg ->
            case model of
                Lobby ({ roomID, gameType } as lobby) ->
                    let
                        ( newLobby, msg ) =
                            Lobby.update lobby lobbyMsg
                    in
                        ( Lobby newLobby, msg )

                otherwise ->
                    ( model, Cmd.none )

        ConnectedMsg connectedMsg ->
            case model of
                Connected connected ->
                    let
                        ( newConnected, cmd ) =
                            Connected.update flags connectedMsg connected
                    in
                        ( Connected newConnected, cmd )

                otherwise ->
                    ( model, Cmd.none )

        LabMsg labMsg ->
            case model of
                Lab lab ->
                    ( Lab <| Lab.update lab labMsg, Cmd.none )

                otherwise ->
                    ( model, Cmd.none )

        StartGame mode ->
            case model of
                Lobby ({ roomID, gameType } as lobby) ->
                    ( Connected <| Connected.init mode roomID
                    , case gameType of
                        Lobby.ComputerGame ->
                            Cmd.none

                        Lobby.CustomGame ->
                            newUrl <| "/play/custom/" ++ roomID

                        Lobby.QuickplayGame ->
                            Cmd.none
                    )

                otherwise ->
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

        Lab lab ->
            ( Lab lab, Cmd.none )


tick : Model -> Float -> Model
tick room dt =
    case room of
        MainMenu ->
            MainMenu

        Lobby lobby ->
            Lobby lobby

        Connected connected ->
            Connected <| Connected.tick connected dt

        Lab lab ->
            Lab <| Lab.tick lab dt
