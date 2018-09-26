module GameState.View exposing (view)

import Animation.State exposing (animToResTickMax)
import Animation.Types exposing (Anim(GameEnd))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import CharacterSelect.View as CharacterSelect
import Connected.Messages as Connected
import Endgame.View as Endgame
import GameState.Messages exposing (..)
import GameState.State exposing (resolvable)
import GameState.Types exposing (GameState(..), PlayState(..), WaitType(..))
import Main.Messages as Main
import Main.Types exposing (Flags)
import Model.Types exposing (..)
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch
import Resolvable.State exposing (activeAnim, activeModel, activeStackCard, resolving)
import Resolvable.Types as Resolvable
import Room.Messages as Room
import Texture.Types as Texture
import Clock.View as Clock


view : GameState -> String -> Flags -> Texture.Model -> Html Main.Msg
view state roomID ({ hostname, httpPort, time, dimensions } as flags) textures =
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

                    -- CLOCK STUFF
                    ( w, h ) =
                        dimensions

                    clock =
                        case started of
                            Playing clock ->
                                clock

                            Ended _ clock _ ->
                                clock

                    resInfo =
                        Just ( res.tick, activeAnim res )

                    resModel =
                        activeModel res

                    stackCard =
                        activeStackCard res

                    clockView =
                        Clock.view params clock textures
                in
                    case started of
                        Playing _ ->
                            div []
                                [ clockView
                                , Endgame.view 0.0 Nothing Nothing
                                ]

                        Ended winner _ mReplayId ->
                            let
                                endAnim =
                                    Just (GameEnd winner)

                                endTick =
                                    animToResTickMax endAnim
                            in
                                div []
                                    [ clockView
                                    , Endgame.view endTick endAnim mReplayId
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
