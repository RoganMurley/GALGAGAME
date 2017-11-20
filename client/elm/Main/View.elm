module Main.View exposing (..)

import Connected.View as Connected
import Html as Html exposing (Html, div, text)
import Lab.View as Lab
import Lobby.View as Lobby
import Main.Types as Main exposing (..)
import Main.Messages exposing (Msg(..))
import Menu.View as Menu
import Raymarch.Types as Raymarch


view : Main.Model -> Html Msg
view ({ hostname, httpPort, frameTime, windowDimensions } as model) =
    let
        params =
            Raymarch.Params frameTime windowDimensions
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
