module Lobby.View exposing (view)

import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Lobby.Messages exposing (Msg(..))
import Lobby.State exposing (gameTypeToString)
import Lobby.Types exposing (GameType(..), Model)


view : Model -> Html Msg
view { error, gameType } =
    div []
        [ div [ class "connecting-box" ]
            [ h1 [] [ text <| gameTypeToString gameType ++ " Game" ]
            , div
                []
                [ div [ class "input-group" ]
                    [ button [ onClick <| GotoLogin ] [ text "Login & Play" ]
                    , div [ class "vertical-rule" ] []
                    , button [ onClick <| JoinRoom ] [ text "Play as Guest" ]
                    ]
                , div [ class "error" ] [ text error ]
                ]
            ]
        ]
