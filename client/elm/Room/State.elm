module Room.State exposing (init, tick, update)

import Connected.State as Connected
import Lab.State as Lab
import Lobby.State as Lobby
import Lobby.Types as Lobby
import Main.Messages exposing (Msg(..))
import Main.Types exposing (Flags)
import Menu.Messages as Menu
import Navigation as Navigation exposing (newUrl)
import Room.Generators exposing (generate)
import Room.Types exposing (..)


init : Model
init =
    MainMenu


update : Model -> Msg -> Flags -> ( Model, Cmd Msg )
update model msg ({ hostname, seed } as flags) =
    case model of
        MainMenu ->
            let
                roomID : String
                roomID =
                    generate Room.Generators.roomID seed
            in
                case msg of
                    MenuMsg (Menu.Start gameType) ->
                        let
                            url : String
                            url =
                                case gameType of
                                    Lobby.ComputerGame ->
                                        "computer"

                                    Lobby.CustomGame ->
                                        "custom"

                                    Lobby.QuickplayGame ->
                                        "quickplay"
                        in
                            ( model, newUrl <| "/play/" ++ url )

                    otherwise ->
                        ( model, Cmd.none )

        Lobby ({ roomID, gameType } as lobby) ->
            case msg of
                StartGame mode ->
                    ( Connected <| Connected.init mode roomID
                    , case gameType of
                        Lobby.ComputerGame ->
                            Cmd.none

                        Lobby.CustomGame ->
                            newUrl <| "/play/custom/" ++ roomID

                        Lobby.QuickplayGame ->
                            Cmd.none
                    )

                LobbyMsg lobbyMsg ->
                    let
                        ( newLobby, msg ) =
                            Lobby.update lobby lobbyMsg
                    in
                        ( Lobby newLobby, msg )

                Receive str ->
                    ( model, Lobby.receive str )

                otherwise ->
                    ( model, Cmd.none )

        Connected connected ->
            let
                ( newConnected, cmd ) =
                    Connected.update flags msg connected
            in
                ( Connected newConnected, cmd )

        Lab lab ->
            case msg of
                LabMsg labMsg ->
                    ( Lab <| Lab.update lab labMsg, Cmd.none )

                otherwise ->
                    ( model, Cmd.none )


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
