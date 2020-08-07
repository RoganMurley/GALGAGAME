module GameState.View exposing (view)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Background.View as Background
import DeckBuilding.View as DeckBuilding
import GameState.Messages exposing (Msg(..))
import GameState.Types exposing (GameState(..), WaitType(..))
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, height, id, readonly, type_, value, width)
import Html.Events exposing (onClick)
import Main.Messages as Main
import Main.Types exposing (Flags)
import PlayState.View as PlayState
import Render.Types as Render
import WebGL


view : GameState -> String -> Flags -> Assets.Model -> Html Main.Msg
view state roomID flags assets =
    let
        ( w, h ) =
            flags.dimensions

        params =
            { time = flags.time, w = w, h = h, pixelRatio = flags.pixelRatio }
    in
    div []
        [ WebGL.toHtml
            [ width <| floor <| toFloat w * flags.pixelRatio
            , height <| floor <| toFloat h * flags.pixelRatio
            , class "webgl-canvas"
            ]
          <|
            webglView state params assets
        , htmlView state roomID flags assets
        ]


htmlView : GameState -> String -> Flags -> Assets.Model -> Html Main.Msg
htmlView state roomID flags assets =
    case state of
        Waiting waitType ->
            div [] [ waitingView waitType flags roomID ]

        Selecting _ ->
            text ""

        Started _ ->
            text ""


waitingView : WaitType -> Flags -> String -> Html Main.Msg
waitingView waitType { httpPort, hostname } roomID =
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
                            [ onClick <| Main.CopyInput myID, class "menu-button" ]
                            [ text "COPY" ]
                        ]

                WaitQuickplay ->
                    text ""
    in
    div [ class "waiting" ]
        [ div [ class "waiting-prompt" ]
            [ text waitingPrompt ]
        , waitingInfo
        ]


webglView : GameState -> Render.Params -> Assets.Model -> List WebGL.Entity
webglView state params assets =
    case state of
        Waiting _ ->
            Background.webglView params assets Finding

        Selecting selecting ->
            DeckBuilding.webglView params selecting assets

        Started started ->
            PlayState.webglView started params assets
