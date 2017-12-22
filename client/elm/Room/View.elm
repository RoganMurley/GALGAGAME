module Room.View exposing (view)

import Connected.View as Connected
import Html as Html exposing (Html, div, text)
import Lab.View as Lab
import Lobby.View as Lobby
import Login.View as Login
import Main.Messages as Main
import Main.Types exposing (Flags)
import Menu.View as Menu
import Raymarch.Types as Raymarch
import Room.Messages exposing (..)
import Room.Types exposing (..)


view : Model -> Flags -> Html Main.Msg
view model flags =
    let
        params =
            Raymarch.Params flags.time flags.dimensions
    in
        case model of
            MainMenu ->
                Html.map (Main.RoomMsg << MenuMsg) <|
                    Menu.view params

            Lobby lobby ->
                Html.map (Main.RoomMsg << LobbyMsg) <|
                    Lobby.view params lobby

            Connected connected ->
                Connected.view connected flags

            Lab lab ->
                Html.map (Main.RoomMsg << LabMsg) <|
                    Lab.view params lab

            Login login ->
                Html.map (Main.RoomMsg << LoginMsg) <|
                    Login.view params login
