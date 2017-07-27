module Main.View exposing (..)

-- import Chat.View as Chat

import Html as Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Settings.View as Settings
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

            Connected { chat, game, settings, roomID, players } ->
                div []
                    [ --Chat.view chat
                      Settings.view settings
                    , playersView players
                    , GameState.view game roomID hostname httpPort frameTime windowDimensions
                    ]


playersView : ( Maybe String, Maybe String ) -> Html msg
playersView ( pa, pb ) =
    let
        playerView : Maybe String -> Bool -> Html msg
        playerView mName other =
            let
                otherClass : String
                otherClass =
                    if other then
                        "other"
                    else
                        ""
            in
                div
                    [ class ("player-name " ++ otherClass) ]
                    [ text <| Maybe.withDefault "" mName ]
    in
        div [ class "player-layer" ]
            [ playerView pb True
            , playerView pa False
            ]
