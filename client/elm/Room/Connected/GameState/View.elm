module GameState.View exposing (view)

import CharacterSelect.View as CharacterSelect
import Connected.Messages as Connected
import Game.State exposing (bareContextInit)
import GameState.Messages exposing (Msg(..))
import GameState.Types exposing (GameState(..), WaitType(..))
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, height, id, readonly, type_, value, width)
import Html.Events exposing (onClick)
import Main.Messages as Main
import Main.Types exposing (Flags)
import PlayState.View as PlayState
import Room.Messages as Room
import Texture.Types as Texture
import WebGL


view : GameState -> String -> Flags -> Texture.Model -> Html Main.Msg
view state roomID flags textures =
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
                    "Searching for Opponent..."

        waitingInfo : Html Main.Msg
        waitingInfo =
            case waitType of
                WaitCustom ->
                    div []
                        [ ringView flags textures
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
                    ringView flags textures
    in
    div [ class "waiting" ]
        [ div [ class "waiting-prompt" ]
            [ text waitingPrompt ]
        , waitingInfo
        ]


ringView : Flags -> Texture.Model -> Html msg
ringView { dimensions } textures =
    let
        ( w, h ) =
            dimensions

        ctx =
            bareContextInit ( w, h ) textures
    in
    div []
        [ WebGL.toHtml
            [ width <| w * 2
            , height <| h * 2
            , class "webgl-canvas"
            ]
          <|
            List.concat <|
                List.map ((|>) ctx)
                    [ CharacterSelect.backgroundRingView
                    , CharacterSelect.circlesView
                    ]
        ]
