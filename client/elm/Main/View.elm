module Main.View exposing (..)

-- import Chat.View as Chat

import Html as Html exposing (Html, div)
import GameModal.View as GameModal
import GameState.View as GameState
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

            Connected { chat, game, modal, roomID } ->
                div []
                    [ --Chat.view chat
                      Html.map GameModalMsg <| GameModal.view modal
                    , GameState.view game roomID hostname httpPort frameTime windowDimensions
                    ]
