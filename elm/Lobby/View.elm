module Lobby.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lobby.Messages as Lobby
import Lobby.Types exposing (..)
import Main.Messages exposing (Msg(..))
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch


view : Raymarch.Params -> Model -> Html Msg
view params { name, error, valid, gameType } =
    let
        gameTypeString : String
        gameTypeString =
            case gameType of
                CustomGame ->
                    "Custom"

                ComputerGame ->
                    "Computer"

        playPrefix : String
        playPrefix =
            case gameType of
                CustomGame ->
                    "play:"

                ComputerGame ->
                    "playComputer:"

        nameInputID : String
        nameInputID =
            "playername-input"
    in
        div []
            [ div [ class "connecting-box" ]
                [ h1 [] [ text <| gameTypeString ++ " Game" ]
                , div []
                    [ div [ class "input-group" ]
                        [ input
                            [ onInput <| LobbyMsg << Lobby.NameInput
                            , placeholder "username"
                            , value name
                            , id nameInputID
                            , onClick <| SelectAllInput nameInputID
                            ]
                            []
                        , button
                            [ onClick <| Send <| playPrefix ++ name
                            , disabled <| not valid
                            ]
                            [ text "Play" ]
                        , button
                            [ onClick <| Send <| "spectate:" ++ name
                            , disabled <| not valid
                            ]
                            [ text "Spec" ]
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
