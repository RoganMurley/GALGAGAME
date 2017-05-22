module Main.View exposing (..)

import Chat.View as Chat
import Html exposing (Html, div)
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
            MainMenu _ ->
                Html.map MenuMsg <| Menu.view params

            Connecting lobby ->
                Lobby.view params lobby

            Connected { chat, game, roomID } ->
                div []
                    [ Chat.view chat
                    , GameState.view game roomID hostname httpPort frameTime windowDimensions
                    ]
