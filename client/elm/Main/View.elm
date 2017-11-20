module Main.View exposing (..)

import Connected.View as Connected
import Html as Html exposing (Html, div, text)
import Lab.View as Lab
import Lobby.View as Lobby
import Main.Types exposing (..)
import Main.Messages exposing (Msg(..))
import Menu.View as Menu
import Raymarch.Types as Raymarch


view : Model -> Html Msg
view model =
    let
        { hostname, httpPort, time, windowDimensions } =
            model.flags

        params =
            Raymarch.Params time windowDimensions
    in
        case model.room of
            MainMenu ->
                Html.map MenuMsg <| Menu.view params

            Connecting lobby ->
                Lobby.view params lobby

            Connected connected ->
                Connected.view connected hostname httpPort params

            Lab lab ->
                Html.map LabMsg <| Lab.view params lab
