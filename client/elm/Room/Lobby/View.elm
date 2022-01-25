module Lobby.View exposing (view)

import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Lobby.Messages exposing (Msg(..))
import Lobby.State exposing (gameTypeToString)
import Lobby.Types exposing (Model)
import Main.Messages as Main exposing (Msg(..))
import Main.Types exposing (Flags)
import Room.Messages exposing (Msg(..))


view : Flags -> Model -> Html Main.Msg
view { username, visits } { error, gameType } =
    case username of
        Nothing ->
            div []
                [ div [ class "connecting-box" ]
                    [ h1 [] [ text <| gameTypeToString gameType ]
                    , div
                        []
                        [ div [ class "input-group" ] <|
                            div
                                [ class "login-buttons" ]
                                [ button
                                    [ onClick GotoSignup, class "menu-button" ]
                                    [ text "SIGNUP & PLAY" ]
                                , button
                                    [ onClick GotoLogin, class "menu-button" ]
                                    [ text "LOGIN & PLAY" ]
                                ]
                                :: guestLoginView visits
                        , div [ class "error" ] [ text error ]
                        ]
                    ]
                ]

        Just _ ->
            text ""


guestLoginView : Int -> List (Html Main.Msg)
guestLoginView visits =
    if visits < 5 then
        [ div [ class "vertical-rule" ] []
        , div [ class "horizontal-rule" ] []
        , button
            [ onClick <| RoomMsg <| LobbyMsg JoinRoom, class "menu-button" ]
            [ text "PLAY AS GUEST" ]
        ]

    else
        []
