module Menu.View exposing (view)

import Html exposing (Html, button, div, h1, h2, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Lobby.State exposing (gameTypeToString)
import Lobby.Types exposing (GameType(..))
import Menu.Messages exposing (Msg(..))


view : Html Msg
view =
    div []
        [ div [ class "main-menu" ]
            [ h1 [] [ text "Ring of Worlds" ]
            , h2 [] [ text "Digital Card Game" ]
            , div [ class "main-menu-buttons" ] <|
                List.map menuButton
                    [ QuickplayGame, CustomGame, ComputerGame ]
            ]
        ]


menuButton : GameType -> Html Msg
menuButton gameType =
    button
        [ class "menu-button"
        , onClick <| Start gameType
        ]
        [ text <| gameTypeToString gameType ]
