module GameState.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import CharacterSelect.View as CharacterSelect
import GameState.Messages as GameState
import GameState.Types exposing (GameState(..))
import Main.Messages exposing (Msg(..))
import Model.Types exposing (..)
import Model.View as Model exposing (view, resView)
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch
import ViewModel.State as ViewModel


view : GameState -> String -> String -> String -> Float -> ( Int, Int ) -> Html Msg
view state roomID hostname httpPort time ( width, height ) =
    let
        params =
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
                        "http://" ++ hostname ++ portProtocol ++ "/play/custom/" ++ roomID

                    myID =
                        "challenge-link"
                in
                    div []
                        [ div [ class "waiting" ]
                            [ div [ class "waiting-prompt" ]
                                [ text "Give this link to your opponent:" ]
                            , div [ class "input-group" ]
                                [ input
                                    [ value challengeLink
                                    , type_ "text"
                                    , readonly True
                                    , id myID
                                    , onClick <| SelectAllInput myID
                                    ]
                                    []
                                , button
                                    [ onClick <| CopyInput myID ]
                                    [ text "copy" ]
                                ]
                            ]
                        , div [] [ Raymarch.view params ]
                        ]

            Selecting model ->
                Html.map (GameStateMsg << GameState.SelectingMsg) <| CharacterSelect.view params model

            PlayingGame ( m, vm ) ( res, resTime ) ->
                div []
                    [ case res of
                        [] ->
                            Model.view resTime ( m, vm ) time

                        otherwise ->
                            resView res resTime ( m, vm ) time
                    , div [] [ Raymarch.view params ]
                    ]

            Ended winner final resModel ( res, resTime ) ->
                case resModel of
                    Just ( m, vm ) ->
                        div []
                            [ resView res resTime ( m, vm ) time
                            , div [] [ Raymarch.view params ]
                            ]

                    Nothing ->
                        let
                            ( endGameText, endGameClass ) =
                                case winner of
                                    Just PlayerA ->
                                        ( "VICTORY", "victory" )

                                    Just PlayerB ->
                                        ( "DEFEAT", "defeat" )

                                    Nothing ->
                                        ( "DRAW", "draw" )
                        in
                            div []
                                [ div [ class ("endgame-layer " ++ endGameClass) ]
                                    [ div [ class "endgame-container" ]
                                        [ div
                                            [ class endGameClass ]
                                            [ text endGameText ]
                                        , button
                                            [ class "rematch", onClick Rematch ]
                                            [ text "Rematch" ]
                                        ]
                                    ]
                                , resView res resTime ( final, ViewModel.init ) time
                                , div [] [ Raymarch.view params ]
                                ]
