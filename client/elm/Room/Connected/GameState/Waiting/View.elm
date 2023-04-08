module Waiting.View exposing (htmlView, webglView)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Background.View as Background
import Game.State exposing (bareContextInit)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, id, readonly, type_, value)
import Html.Events exposing (onClick)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Math.Matrix4 exposing (makeScale3)
import Math.Vector3 exposing (vec3)
import Mouse exposing (MouseState(..))
import Quaternion
import Render.Primitives
import Render.Shaders
import Render.Types as Render
import Waiting.Types exposing (Model, WaitType(..))
import WebGL


htmlView : Model -> Flags -> String -> Html Main.Msg
htmlView { waitType } { httpPort, hostname } roomID =
    case waitType of
        Just WaitCustom ->
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
            div [ class "waiting" ]
                [ div [ class "waiting-prompt" ]
                    [ text waitingPrompt ]
                , waitingInfo
                ]

        Just WaitChallenge ->
            div [ class "waiting" ]
                [ div [ class "waiting-prompt" ]
                    [ text "WAITING FOR OPPONENT TO JOIN..."
                    ]
                ]

        _ ->
            text ""


webglView : Model -> Render.Params -> Assets.Model -> List WebGL.Entity
webglView { bounceTick, bulge, waitType } params assets =
    let
        ctx =
            bareContextInit ( params.w, params.h ) assets NoMouse

        { w, h, camera2d, ortho, radius } =
            ctx

        size =
            radius * 0.7 + bulge

        rot =
            0.003 * params.time

        mag =
            0.8
                + (0.18
                    * sin (bounceTick * 0.002)
                  )
    in
    List.concat
        [ Background.webglView params assets Finding
        ]
        ++ (case waitType of
                Just WaitQuickplay ->
                    [ Render.Primitives.quad Render.Shaders.donutFragment <|
                        { rotation = Quaternion.makeRotate <| Quaternion.zRotation 0
                        , scale = makeScale3 size size 1
                        , color = vec3 (70 / 255) (70 / 255) (200 / 255)
                        , pos = vec3 (w * 0.5) (h * 0.5) 0
                        , perspective = ortho
                        , camera = camera2d
                        , mag = 0
                        , thickness = 0.2
                        }
                    , Render.Primitives.quad Render.Shaders.donutFragment <|
                        { rotation = Quaternion.makeRotate <| Quaternion.zRotation pi
                        , scale = makeScale3 size size 1
                        , color = vec3 (70 / 255) (70 / 255) (200 / 255)
                        , pos = vec3 (w * 0.5) (h * 0.5) 0
                        , perspective = ortho
                        , camera = camera2d
                        , mag = 0
                        , thickness = 0.2
                        }
                    , Render.Primitives.quad Render.Shaders.donutFragment <|
                        { rotation = Quaternion.makeRotate <| Quaternion.zRotation rot
                        , scale = makeScale3 size size 1
                        , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                        , pos = vec3 (w * 0.5) (h * 0.5) 0
                        , perspective = ortho
                        , camera = camera2d
                        , mag = mag
                        , thickness = 0.2
                        }
                    , Render.Primitives.quad Render.Shaders.donutFragment <|
                        { rotation = Quaternion.makeRotate <| Quaternion.zRotation (rot + pi)
                        , scale = makeScale3 size size 1
                        , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                        , pos = vec3 (w * 0.5) (h * 0.5) 0
                        , perspective = ortho
                        , camera = camera2d
                        , mag = mag
                        , thickness = 0.2
                        }
                    ]

                _ ->
                    []
           )
