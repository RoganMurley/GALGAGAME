module Menu.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lobby.State exposing (gameTypeToString)
import Lobby.Types exposing (GameType(..))
import Menu.Messages exposing (Msg(..))
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch


view : Raymarch.Params -> Html Msg
view params =
    div []
        [ div [ class "main-menu" ]
            [ h1 [] [ text "HUBRIS" ]
            , h2 [] [ text "A card game of risk & reward" ]
            , div [ class "main-menu-buttons" ] <|
                List.map menuButton
                    [ QuickplayGame, CustomGame, ComputerGame ]
            ]
        , div [] [ Raymarch.view params ]
        ]


menuButton : GameType -> Html Msg
menuButton gameType =
    button
        [ class "menu-button"
        , onClick <| Start gameType
        ]
        [ text <| gameTypeToString gameType ]
