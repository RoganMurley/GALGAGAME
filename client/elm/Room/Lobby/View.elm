module Lobby.View exposing (view)

import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Lobby.Messages exposing (Msg(..))
import Lobby.State exposing (gameTypeToString)
import Lobby.Types exposing (Model)


view : Model -> Html Msg
view { error, gameType } =
    div []
        [ div [ class "connecting-box" ]
            [ h1 [] [ text <| gameTypeToString gameType ]
            , div
                []
                [ div [ class "input-group" ]
                    [ div [ class "login-buttons" ]
                        [ button
                            [ onClick <| GotoSignup ]
                            [ text "SIGNUP & PLAY" ]
                        , button
                            [ onClick <| GotoLogin ]
                            [ text "LOGIN & PLAY" ]
                        ]
                    , div [ class "vertical-rule" ] []
                    , button [ onClick <| JoinRoom ] [ text "PLAY AS GUEST" ]
                    ]
                , div [ class "error" ] [ text error ]
                ]
            ]
        ]
