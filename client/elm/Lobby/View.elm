module Lobby.View exposing (view)

import Connected.Types exposing (Mode(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lobby.Messages exposing (Msg(..))
import Lobby.Types exposing (..)
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch


view : Raymarch.Params -> Model -> Html Msg
view params { error, gameType } =
    div []
        [ div [ class "connecting-box" ]
            [ h1 [] [ text <| (gameTypeStr gameType) ++ " Game" ]
            , div []
                [ div [ class "input-group" ]
                    [ button
                        [ onClick <| JoinRoom Playing
                        ]
                        [ text "Login & Play" ]
                    , div [ class "vertical-rule" ] []
                    , button
                        [ onClick <| JoinRoom Playing
                        ]
                        [ text "Play as Guest" ]
                    ]
                , div
                    [ class "error" ]
                    [ text error ]
                ]
            ]
        , div
            []
            [ Raymarch.view params ]
        ]


gameTypeStr : GameType -> String
gameTypeStr gameType =
    case gameType of
        CustomGame ->
            "Custom"

        ComputerGame ->
            "Computer"

        QuickplayGame ->
            "Quickplay"
