module Menu.View exposing (view)

import GameType exposing (GameType(..))
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Lobby.State exposing (gameTypeToString)
import Menu.Messages exposing (Msg(..))


view : Html Msg
view =
    div []
        [ div [ class "main-menu" ]
            [ h1 [] [ text "GALGA" ]
            , div [ class "main-menu-buttons" ] <|
                List.map menuButton
                    [ QuickplayGame, CustomGame ]
            ]
        ]


menuButton : GameType -> Html Msg
menuButton gameType =
    button
        [ onClick <| Start gameType, class "menu-button" ]
        [ text <| gameTypeToString gameType ]
