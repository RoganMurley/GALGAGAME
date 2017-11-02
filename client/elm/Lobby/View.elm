module Lobby.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lobby.Messages as Lobby
import Lobby.Types exposing (..)
import Main.Messages exposing (Msg(..))
import Main.Types exposing (Mode(..))
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch


view : Raymarch.Params -> Model -> Html Msg
view params { error, gameType } =
    let
        gameTypeString : String
        gameTypeString =
            case gameType of
                CustomGame ->
                    "Custom"

                ComputerGame ->
                    "Computer"

                QuickplayGame ->
                    "Quickplay"

        nameInputID : String
        nameInputID =
            "playername-input"
    in
        div []
            [ div [ class "connecting-box" ]
                [ h1 [] [ text <| gameTypeString ++ " Game" ]
                , div []
                    [ div [ class "input-group" ]
                        [ button
                            [ onClick <| LobbyMsg <| Lobby.JoinRoom Playing
                            ]
                            [ text "Login & Play" ]
                        , div [ class "vertical-rule" ] []
                        , button
                            [ onClick <| LobbyMsg <| Lobby.JoinRoom Playing
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
