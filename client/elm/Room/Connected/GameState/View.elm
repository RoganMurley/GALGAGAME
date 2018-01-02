module GameState.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import CharacterSelect.View as CharacterSelect
import Connected.Messages as Connected
import GameState.Messages exposing (..)
import GameState.State exposing (activeAnim)
import GameState.Types exposing (GameState(..), WaitType(..))
import Main.Messages as Main
import Main.Types exposing (Flags)
import Model.Types exposing (..)
import Model.View as Model exposing (view, resView)
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch
import Room.Messages as Room
import Animation.View as Animation


view : GameState -> String -> Flags -> Html Main.Msg
view state roomID { hostname, httpPort, time, dimensions } =
    let
        params =
            Raymarch.Params time dimensions
    in
        case state of
            Waiting waitType ->
                div []
                    [ waitingView waitType httpPort hostname roomID
                    , Raymarch.view params
                    ]

            Selecting model ->
                Html.map
                    (Main.RoomMsg
                        << Room.ConnectedMsg
                        << Connected.GameStateMsg
                        << SelectingMsg
                    )
                <|
                    CharacterSelect.view params model

            PlayingGame ( m, vm ) ( res, resTime ) ->
                div []
                    (case res of
                        [] ->
                            [ Model.view resTime ( m, vm ) time
                            , Raymarch.view params
                            ]

                        otherwise ->
                            [ resView res resTime ( m, vm ) time
                            , Animation.view params resTime (activeAnim state)
                            ]
                    )

            Ended winner final vm resModel ( res, resTime ) ->
                case resModel of
                    Just m ->
                        div []
                            [ resView (res ++ [ final ]) resTime ( m, vm ) time
                            , Animation.view params resTime (activeAnim state)
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
                                            [ class "rematch"
                                            , onClick <|
                                                Main.RoomMsg <|
                                                    Room.ConnectedMsg <|
                                                        Connected.GameStateMsg <|
                                                            PlayingOnly <|
                                                                Rematch
                                            ]
                                            [ text "Rematch" ]
                                        ]
                                    ]
                                , resView res resTime ( final, vm ) time
                                , Animation.view params resTime (activeAnim state)
                                ]


waitingView : WaitType -> String -> String -> String -> Html Main.Msg
waitingView waitType httpPort hostname roomID =
    let
        portProtocol =
            if httpPort /= "" then
                ":" ++ httpPort
            else
                ""

        challengeLink =
            "https://" ++ hostname ++ portProtocol ++ "/play/custom/" ++ roomID

        myID =
            "challenge-link"

        waitingPrompt =
            case waitType of
                WaitCustom ->
                    "Give this link to your opponent:"

                WaitQuickplay ->
                    "Searching for opponent"

        waitingInfo : Html Main.Msg
        waitingInfo =
            case waitType of
                WaitCustom ->
                    div [ class "input-group" ]
                        [ input
                            [ value challengeLink
                            , type_ "text"
                            , readonly True
                            , id myID
                            , onClick <| Main.SelectAllInput myID
                            ]
                            []
                        , button
                            [ onClick <| Main.CopyInput myID ]
                            [ text "copy" ]
                        ]

                WaitQuickplay ->
                    div []
                        [ div [ class "lds-facebook" ]
                            [ div [] []
                            , div [] []
                            , div [] []
                            ]
                        ]
    in
        div [ class "waiting" ]
            [ div [ class "waiting-prompt" ]
                [ text waitingPrompt ]
            , waitingInfo
            ]
