module GameState.View exposing (view)

import Animation.State exposing (animToResTickMax)
import Animation.Types exposing (Anim(GameEnd))
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, id, readonly, type_, value)
import Html.Events exposing (onClick)
import CharacterSelect.View as CharacterSelect
import Connected.Messages as Connected
import Endgame.View as Endgame
import GameState.Messages exposing (Msg(..))
import GameState.Types exposing (GameState(..), PlayState(..), WaitType(..))
import Main.Messages as Main
import Main.Types exposing (Flags)
import Room.Messages as Room
import Texture.Types as Texture
import Clock.View as Clock


view : GameState -> String -> Flags -> Texture.Model -> Html Main.Msg
view state roomID { hostname, httpPort, time, dimensions } textures =
    let
        ( w, h ) =
            dimensions

        params =
            { time = time, w = w, h = h }
    in
        case state of
            Waiting waitType ->
                div []
                    [ waitingView waitType httpPort hostname roomID
                    ]

            Selecting model ->
                Html.map
                    (Main.RoomMsg
                        << Room.ConnectedMsg
                        << Connected.GameStateMsg
                        << SelectingMsg
                    )
                <|
                    CharacterSelect.view model

            Started started ->
                case started of
                    Playing clock ->
                        div []
                            [ Clock.view params clock textures
                            , Endgame.view 0.0 Nothing Nothing
                            ]

                    Ended winner clock mReplayId ->
                        let
                            resolving =
                                not <| List.isEmpty clock.res.resList

                            endAnim =
                                if resolving then
                                    Nothing
                                else
                                    Just <| GameEnd winner

                            endTick =
                                if resolving then
                                    0
                                else
                                    animToResTickMax endAnim
                        in
                            div []
                                [ Clock.view params clock textures
                                , Endgame.view
                                    endTick
                                    endAnim
                                    mReplayId
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
