module Menu.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lobby.Types as Lobby
import Menu.Messages exposing (Msg(..))
import Raymarch.Types as Raymarch


-- import Raymarch.View as Raymarch

import Animation.View as Animation


view : Raymarch.Params -> Html Msg
view params =
    div []
        [ div [ class "main-menu" ]
            [ h1 [] [ text "HUBRIS" ]
            , h2 [] [ text "A card game of risk & reward" ]
            , div [ class "main-menu-buttons" ]
                [ button
                    [ class "menu-button"
                    , onClick <| Start Lobby.QuickplayGame
                    ]
                    [ text "Quickplay" ]
                , button
                    [ class "menu-button"
                    , onClick <| Start Lobby.CustomGame
                    ]
                    [ text "Custom" ]
                , button
                    [ class "menu-button"
                    , onClick <| Start Lobby.ComputerGame
                    ]
                    [ text "Computer" ]
                ]
            ]
        , div [] [ Animation.view params ]
        ]
