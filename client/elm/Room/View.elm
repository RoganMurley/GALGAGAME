module Room.View exposing (view)

import Connected.View as Connected
import Html as Html exposing (Html, div, text)
import Lab.View as Lab
import Lobby.View as Lobby
import Main.Messages exposing (Msg(..))
import Main.Types exposing (Flags)
import Menu.View as Menu
import Raymarch.Types as Raymarch
import Room.Types exposing (..)


view : Model -> Flags -> Html Msg
view model flags =
    let
        params =
            Raymarch.Params flags.time flags.dimensions
    in
        case model of
            MainMenu ->
                Html.map MenuMsg <| Menu.view params

            Lobby lobby ->
                Lobby.view params lobby

            Connected connected ->
                Connected.view connected flags

            Lab lab ->
                Html.map LabMsg <| Lab.view params lab
