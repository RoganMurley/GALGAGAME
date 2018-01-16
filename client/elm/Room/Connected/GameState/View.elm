module GameState.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Animation.Types exposing (Anim)
import CharacterSelect.View as CharacterSelect
import Connected.Messages as Connected
import GameState.Messages exposing (..)
import GameState.State exposing (resolvable)
import GameState.Types exposing (GameState(..), PlayState(..), WaitType(..))
import Main.Messages as Main
import Main.Types exposing (Flags)
import Model.Types exposing (..)
import Model.View as Model exposing (view, resView)
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch
import Resolvable.State exposing (activeAnim, resolving)
import Resolvable.Types as Resolvable
import Room.Messages as Room
import Animation.View as Animation


view : GameState -> String -> Flags -> Html Main.Msg
view state roomID ({ hostname, httpPort, time, dimensions } as flags) =
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

            Started started ->
                let
                    res : Resolvable.Model
                    res =
                        resolvable started

                    anim : Maybe Anim
                    anim =
                        activeAnim res
                in
                    case res.resList of
                        resData :: _ ->
                            div []
                                [ resView res.vm resData time
                                , Animation.view params res.tick anim
                                ]

                        otherwise ->
                            let
                                model : Model
                                model =
                                    res.final
                            in
                                case started of
                                    Playing _ ->
                                        div []
                                            [ Model.view ( model, res.vm ) time
                                            , Raymarch.view params
                                            ]

                                    Ended winner _ ->
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
                                                , view (Started (Playing res)) roomID flags
                                                , Animation.view params res.tick anim
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
