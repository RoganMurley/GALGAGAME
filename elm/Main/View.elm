module Main.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Chat.View as Chat
import GameState.View as GameState
import Menu.View as Menu
import Main.Types as Main exposing (..)
import Main.Messages exposing (Msg(..))
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch


view : Main.Model -> Html Msg
view ({ hostname, httpPort, frameTime, windowDimensions } as model) =
    case model.room of
        MainMenu _ ->
            Html.map MenuMsg <|
                Menu.view <|
                    Raymarch.Params frameTime windowDimensions

        Connected { chat, game, roomID } ->
            div []
                [ Chat.view chat
                , GameState.view game roomID hostname httpPort frameTime windowDimensions
                ]

        Connecting { name, error, valid, gameType } ->
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
            in
                div []
                    [ div [ class "connecting-box" ]
                        [ h1 [] [ text (gameTypeString ++ " Game") ]
                        , div []
                            [ div [ class "input-group" ]
                                [ input [ onInput Input, placeholder "username", value name, id "playername-input", onClick (SelectAllInput "playername-input") ] []
                                , button
                                    [ onClick (Send (playPrefix ++ name))
                                    , disabled (not valid)
                                    ]
                                    [ text "Play" ]
                                , button
                                    [ onClick (Send ("spectate:" ++ name))
                                    , disabled (not valid)
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
                        [ Raymarch.view (Raymarch.Params frameTime windowDimensions) ]
                    ]
