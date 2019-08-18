module GameState.View exposing (view)

import Animation.Types exposing (Anim(..))
import Background.View as Background
import CharacterSelect.View as CharacterSelect
import Connected.Messages as Connected
import GameState.Messages exposing (Msg(..))
import GameState.Types exposing (GameState(..), WaitType(..))
import GameType exposing (GameType)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, id, readonly, type_, value)
import Html.Events exposing (onClick)
import Main.Messages as Main
import Main.Types exposing (Flags)
import PlayState.View as PlayState
import Room.Messages as Room
import Texture.Types as Texture


view : GameState -> String -> Flags -> Maybe GameType -> Bool -> Texture.Model -> Html Main.Msg
view state roomID flags gameType isReplay textures =
    case state of
        Waiting waitType ->
            div [] [ waitingView waitType flags textures roomID ]

        Selecting model ->
            let
                ( w, h ) =
                    flags.dimensions

                params =
                    { w = w
                    , h = h
                    , time = flags.time
                    , pixelRatio = flags.pixelRatio
                    }
            in
            Html.map
                (Main.RoomMsg
                    << Room.ConnectedMsg
                    << Connected.GameStateMsg
                    << SelectingMsg
                )
            <|
                CharacterSelect.view params model textures

        Started playState ->
            PlayState.view playState flags gameType isReplay textures


waitingView : WaitType -> Flags -> Texture.Model -> String -> Html Main.Msg
waitingView waitType ({ httpPort, hostname } as flags) textures roomID =
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
                    "Give this link to your Opponent"

                WaitQuickplay ->
                    "Finding Opponent"

        waitingInfo : Html Main.Msg
        waitingInfo =
            case waitType of
                WaitCustom ->
                    div []
                        [ Background.view flags textures Finding
                        , div [ class "input-group" ]
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
                        ]

                WaitQuickplay ->
                    Background.view flags textures Finding
    in
    div [ class "waiting" ]
        [ div [ class "waiting-prompt" ]
            [ text waitingPrompt ]
        , waitingInfo
        ]
