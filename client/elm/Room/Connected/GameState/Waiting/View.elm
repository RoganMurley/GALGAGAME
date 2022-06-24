module Waiting.View exposing (htmlView, webglView)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Background.View as Background
import Font.View as Font
import Game.State exposing (bareContextInit)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, id, readonly, type_, value)
import Html.Events exposing (onClick)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Math.Vector3 exposing (vec3)
import Mouse exposing (MouseState(..))
import Random
import Random.List as Random
import Render.Types as Render
import Waiting.Types exposing (Model, WaitType(..))
import WebGL


htmlView : Model -> Flags -> String -> Html Main.Msg
htmlView { waitType } { httpPort, hostname } roomID =
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
            "Give this link to your opponent"

        waitingInfo : Html Main.Msg
        waitingInfo =
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
    in
    case waitType of
        WaitQuickplay ->
            text ""

        WaitCustom ->
            div [ class "waiting" ]
                [ div [ class "waiting-prompt" ]
                    [ text waitingPrompt ]
                , waitingInfo
                ]


webglView : Model -> Render.Params -> Assets.Model -> List WebGL.Entity
webglView { bounceTick, waitType, seed } params assets =
    let
        ctx =
            bareContextInit ( params.w, params.h ) assets NoMouse

        { w, h } =
            ctx

        size =
            1.4 * max w h

        textMessages =
            [ "ALIGNING\nFATES..."
            , "RESOLVING\nPROPHECIES..."
            , "BRANCHING\nDESTINIES..."
            ]

        mWaitingMessage =
            case seed of
                Just s ->
                    Tuple.first <|
                        Tuple.first <|
                            Random.step
                                (Random.choose textMessages)
                                s

                Nothing ->
                    Nothing
    in
    List.concat
        [ Background.webglView params assets Finding
        ]
        ++ (case ( waitType, mWaitingMessage ) of
                ( WaitQuickplay, Just waitingMessage ) ->
                    Font.view
                        "Futura"
                        waitingMessage
                        { x = w * 0.5 - 0.003 * size
                        , y = h * 0.4
                        , scaleX = 0.0001 * size + 0.003 * sin (bounceTick * 0.005)
                        , scaleY = 0.0001 * size + 0.003 * sin (bounceTick * 0.007)
                        , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                        }
                        ctx

                _ ->
                    []
           )
