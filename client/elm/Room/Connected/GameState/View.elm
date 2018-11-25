module GameState.View exposing (view)

import CharacterSelect.View as CharacterSelect
import Connected.Messages as Connected
import GameState.Messages exposing (Msg(..))
import GameState.Types exposing (GameState(..), WaitType(..))
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, id, readonly, type_, value)
import Html.Events exposing (onClick)
import Main.Messages as Main
import Main.Types exposing (Flags)
import PlayState.View as PlayState
import Room.Messages as Room
import Texture.Types as Texture


view : GameState -> String -> Flags -> Texture.Model -> Html Main.Msg
view state roomID ({ hostname, httpPort } as flags) textures =
    case state of
        Waiting waitType ->
            div [] [ waitingView waitType httpPort hostname roomID ]

        Selecting model ->
            let
                ( w, h ) =
                    flags.dimensions

                params =
                    { w = w
                    , h = h
                    , time = flags.time
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
            PlayState.view playState flags textures


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
                    "Searching for Opponent"

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
