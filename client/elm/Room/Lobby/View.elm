module Lobby.View exposing (view)

import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Lobby.Messages exposing (Msg(..))
import Lobby.State exposing (gameTypeToString)
import Lobby.Types exposing (Model)
import Main.Types exposing (Flags)


view : Flags -> Model -> Html Msg
view { username, visits } { error, gameType } =
    case username of
        Nothing ->
            div []
                [ div [ class "connecting-box" ]
                    [ h1 [] [ text <| gameTypeToString gameType ]
                    , div
                        []
                        [ div [ class "input-group" ] <|
                            [ div
                                [ class "login-buttons" ]
                                [ button
                                    [ onClick <| GotoSignup, class "menu-button" ]
                                    [ text "SIGNUP & PLAY" ]
                                , button
                                    [ onClick <| GotoLogin, class "menu-button" ]
                                    [ text "LOGIN & PLAY" ]
                                ]
                            ]
                                ++ guestLoginView visits
                        , div [ class "error" ] [ text error ]
                        ]
                    ]
                ]

        Just _ ->
            text ""


guestLoginView : Int -> List (Html Msg)
guestLoginView visits =
    if visits < 5 then
        [ div [ class "vertical-rule" ] []
        , div [ class "horizontal-rule" ] []
        , button [ onClick <| JoinRoom, class "menu-button" ] [ text "PLAY AS GUEST" ]
        ]

    else
        []
