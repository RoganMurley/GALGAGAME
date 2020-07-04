module GameState.View exposing (view)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Background.View as Background
import Connected.Messages as Connected
import DeckBuilding.View as DeckBuilding
import GameState.Messages exposing (Msg(..))
import GameState.Types exposing (GameState(..), WaitType(..))
import GameType exposing (GameType)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, height, id, readonly, type_, value, width)
import Html.Events exposing (onClick)
import Main.Messages as Main
import Main.Types exposing (Flags)
import PlayState.View as PlayState
import Room.Messages as Room
import WebGL


view : GameState -> String -> Flags -> Maybe GameType -> Bool -> Assets.Model -> Html Main.Msg
view state roomID ({ dimensions, pixelRatio } as flags) gameType isReplay assets =
    let
        ( w, h ) =
            dimensions

        params =
            { w = w
            , h = h
            , time = flags.time
            , pixelRatio = flags.pixelRatio
            }
    in
    case state of
        Waiting waitType ->
            div [] [ waitingView waitType flags assets roomID ]

        Selecting model ->
            Html.map
                (Main.RoomMsg
                    << Room.ConnectedMsg
                    << Connected.GameStateMsg
                    << SelectingMsg
                )
            <|
                DeckBuilding.view params model assets

        Started playState ->
            div [ class "clock" ]
                [ WebGL.toHtml
                    [ width <| floor <| toFloat w * pixelRatio
                    , height <| floor <| toFloat h * pixelRatio
                    , class "webgl-canvas"
                    ]
                  <|
                    PlayState.view playState flags gameType isReplay assets
                ]


waitingView : WaitType -> Flags -> Assets.Model -> String -> Html Main.Msg
waitingView waitType ({ httpPort, hostname } as flags) assets roomID =
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
                        [ Background.view flags assets Finding
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
                                [ onClick <| Main.CopyInput myID, class "menu-button" ]
                                [ text "COPY" ]
                            ]
                        ]

                WaitQuickplay ->
                    Background.view flags assets Finding
    in
    div [ class "waiting" ]
        [ div [ class "waiting-prompt" ]
            [ text waitingPrompt ]
        , waitingInfo
        ]
