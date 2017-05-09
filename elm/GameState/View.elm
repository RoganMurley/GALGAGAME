module GameState.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Card exposing (Card)
import CharacterSelect.State as CharacterSelect
import CharacterSelect.Types as CharacterSelect
import CharacterSelect.View as CharacterSelect
import GameState.Types exposing (GameState(..), fullify, unfullify)
import GameState.Messages as GameState
import Messages exposing (Msg(..))
import Model.State exposing (..)
import Model.Types exposing (..)
import Model.View as Model exposing (view, resView)
import Vfx
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch


view : GameState -> String -> String -> String -> Float -> ( Int, Int ) -> Html Msg
view state roomID hostname httpPort time ( width, height ) =
    let
        params =
            Vfx.Params time ( width, height )

        rParams =
            Raymarch.Params time ( width, height )
    in
        case state of
            Waiting ->
                let
                    portProtocol =
                        if httpPort /= "" then
                            ":" ++ httpPort
                        else
                            ""

                    challengeLink =
                        "http://" ++ hostname ++ portProtocol ++ "?play=" ++ roomID

                    myID =
                        "challenge-link"
                in
                    div []
                        [ div [ class "waiting" ]
                            [ div [ class "waiting-prompt" ] [ text "Give this link to your opponent:" ]
                            , div [ class "input-group" ]
                                [ input [ value challengeLink, type_ "text", readonly True, id myID, onClick (SelectAllInput myID) ] []
                                , button [ onClick (CopyInput myID) ] [ text "copy" ]
                                ]
                            ]
                        , div [] [ Raymarch.view rParams ]
                        ]

            Selecting model ->
                CharacterSelect.view rParams model

            PlayingGame m ( res, resTime ) ->
                div []
                    [ (case res of
                        [] ->
                            Model.view params resTime m

                        otherwise ->
                            resView params res resTime m
                      )
                    , div [] [ Raymarch.view rParams ]
                    ]

            Ended winner model ( res, resTime ) ->
                case model of
                    Just m ->
                        div []
                            [ resView params res resTime m
                            , div [] [ Raymarch.view rParams ]
                            ]

                    Nothing ->
                        div []
                            [ div [ class "endgame" ]
                                (case winner of
                                    Nothing ->
                                        [ div [ class "draw" ] [ text "DRAW" ]
                                        , button [ class "rematch", onClick Rematch ] [ text "Rematch" ]
                                        ]

                                    Just player ->
                                        [ if player == PlayerA then
                                            div [ class "victory" ] [ text "VICTORY" ]
                                          else
                                            div [ class "defeat" ] [ text "DEFEAT" ]
                                        , button [ class "rematch", onClick Rematch ] [ text "Rematch" ]
                                        ]
                                )
                            , div [] [ Raymarch.view rParams ]
                            ]
