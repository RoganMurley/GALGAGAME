module GameState.View exposing (view)

import Animation.State as Animation
import Animation.Types exposing (Anim(GameEnd, NullAnim))
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
import Model.View as Model
import Resolvable.State exposing (activeAnim)
import Room.Messages as Room
import Texture.Types as Texture


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
                    Playing game ->
                        div []
                            [ Model.view params game textures
                            , Endgame.view 0 NullAnim Nothing
                            ]

                    Ended winner game mReplayId ->
                        let
                            anim =
                                activeAnim game.res

                            { resList, tick } =
                                game.res

                            resolving =
                                not <| List.isEmpty resList

                            ( endAnim, progress ) =
                                if resolving then
                                    ( anim, Animation.progress anim tick )
                                else
                                    ( GameEnd winner, 1.0 )
                        in
                            div []
                                [ Model.view params game textures
                                , Endgame.view progress endAnim mReplayId
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
