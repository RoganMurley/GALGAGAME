module Menu.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Main.Types exposing (GameType(..))
import Menu.Messages exposing (Msg(..))
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch


view : Raymarch.Params -> Html Msg
view params =
    div []
        [ div [ class "main-menu" ]
            [ h1 [] [ text "STØRMCARDS" ]
            , h2 [] [ text "A digital card game of risk & reward" ]
            , div [ class "main-menu-buttons" ]
                [ button
                    [ class "menu-button", disabled True ]
                    [ text "Quickplay" ]
                , button
                    [ class "menu-button"
                    , onClick <| Start <| CustomGame
                    ]
                    [ text "Custom" ]
                , button
                    [ class "menu-button"
                    , onClick <| Start <| ComputerGame
                    ]
                    [ text "Computer" ]
                ]
            ]
        , div [] [ Raymarch.view params ]
        ]
