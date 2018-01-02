module Lobby.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lobby.Messages exposing (Msg(..))
import Lobby.State exposing (gameTypeToString)
import Lobby.Types exposing (..)
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch


view : Raymarch.Params -> Model -> Html Msg
view params { error, gameType } =
    div []
        [ div [ class "connecting-box" ]
            [ h1 [] [ text <| (gameTypeToString gameType) ++ " Game" ]
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
        , div
            []
            [ Raymarch.view params ]
        ]
