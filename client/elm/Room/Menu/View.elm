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
            [ h1 [] [ text "FUTURE TEAROOM" ]
            , div [ class "main-menu-buttons" ] <|
                List.map menuButton
                    [ TutorialGame, QuickplayGame, CustomGame, ComputerGame ]
            ]
        ]


menuButton : GameType -> Html Msg
menuButton gameType =
    button
        [ onClick <| Start gameType ]
        [ text <| gameTypeToString gameType ]
