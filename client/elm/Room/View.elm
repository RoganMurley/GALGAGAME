module Room.View exposing (view)

import Connected.View as Connected
import Html as Html exposing (Html, div)
import Lobby.View as Lobby
import Login.View as Login
import Main.Messages as Main
import Main.Types exposing (Flags)
import Menu.View as Menu
import Replay.View as Replay
import Room.Messages exposing (Msg(..))
import Room.Types exposing (Model(..))
import Settings.Types as Settings
import Settings.View as Settings
import Texture.Types as Texture


view : Model -> Settings.Model -> Flags -> Texture.Model -> Html Main.Msg
view model settings flags textures =
    let
        roomView =
            case model of
                MainMenu ->
                    Html.map (Main.RoomMsg << MenuMsg) <|
                        Menu.view

                Lobby lobby ->
                    Html.map (Main.RoomMsg << LobbyMsg) <|
                        Lobby.view lobby

                Connected connected ->
                    Connected.view connected flags textures

                Replay replay ->
                    Replay.view replay flags textures

                Login login ->
                    Html.map (Main.RoomMsg << LoginMsg) <|
                        Login.view login
    in
        div []
            [ Settings.view settings (settingsView model flags)
            , roomView
            ]


settingsView : Model -> Flags -> List (Html Main.Msg)
settingsView model flags =
    let
        baseViews : List (Html Main.Msg)
        baseViews =
            Login.logoutView flags
    in
        case model of
            Connected { game } ->
                List.concat
                    [ baseViews
                    , List.map (Html.map (Main.RoomMsg << ConnectedMsg)) <|
                        Connected.concedeView game
                    ]

            _ ->
                baseViews
