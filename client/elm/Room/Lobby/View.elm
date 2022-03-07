module Lobby.View exposing (view)

import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Lobby.Messages exposing (Msg(..))
import Lobby.State exposing (gameTypeToString)
import Lobby.Types exposing (Model)
import Main.Messages as Main exposing (Msg(..))
import Main.Types exposing (Flags)
import Mode exposing (Mode(..))
import Room.Messages exposing (Msg(..))


view : Flags -> Model -> Html Main.Msg
view { username } { error, gameType, mode } =
    let
        ( headerText, modeText ) =
            case mode of
                Spectating ->
                    ( "Spectate", "SPEC" )

                Playing ->
                    ( gameTypeToString gameType, "PLAY" )
    in
    case username of
        Nothing ->
            div []
                [ div [ class "connecting-box" ]
                    [ h1 [] [ text headerText ]
                    , div
                        []
                        [ div [ class "input-group" ]
                            [ div
                                [ class "login-buttons" ]
                                [ button
                                    [ onClick GotoSignup, class "menu-button" ]
                                    [ text <| "SIGNUP & " ++ modeText ]
                                , button
                                    [ onClick GotoLogin, class "menu-button" ]
                                    [ text <| "LOGIN & " ++ modeText ]
                                ]
                            , div [ class "vertical-rule" ] []
                            , div [ class "horizontal-rule" ] []
                            , button
                                [ onClick <| RoomMsg <| LobbyMsg JoinRoom, class "menu-button" ]
                                [ text <| modeText ++ " AS GUEST" ]
                            ]
                        , div [ class "error" ] [ text error ]
                        ]
                    ]
                ]

        Just _ ->
            text ""
