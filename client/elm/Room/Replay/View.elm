module Replay.View exposing (htmlView, webglView)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Background.View as Background
import Chat.State as Chat
import Connected.View exposing (playersView)
import Font.View as Font
import Game.State exposing (bareContextInit)
import GameState.Types exposing (GameState(..))
import GameState.View as GameState
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (on, onClick, onMouseDown, onMouseLeave, onMouseUp)
import Json.Decode
import Main.Types exposing (Flags)
import Math.Vector3 exposing (vec3)
import Mouse
import Render.Types as Render
import Replay.Decoders exposing (dragEventDecoder)
import Replay.Messages exposing (Msg(..))
import Replay.Types exposing (Model)
import Set
import Util exposing (px)
import WebGL


htmlView : Model -> Html Msg
htmlView model =
    case model.replay of
        Just { usernamePa, usernamePb } ->
            div []
                [ playersView
                    { pa =
                        Just
                            { name = usernamePa
                            , xp = 0
                            , unlocks = Set.empty
                            , quests = []
                            }
                    , pb =
                        Just
                            { name = usernamePb
                            , xp = 0
                            , unlocks = Set.empty
                            , quests = []
                            }
                    }
                , controlsView model
                ]

        Nothing ->
            div [ class "error" ] [ text model.error ]


controlsView : Model -> Html Msg
controlsView model =
    let
        { playing } =
            model

        rewind =
            button
                [ onMouseDown <| SetReverse True
                , on "touchstart" <| Json.Decode.succeed <| SetReverse True
                , onMouseUp <| SetReverse False

                -- , on "touchend" <| Json.Decode.succeed <| SetReverse False
                , on "touchcancel" <| Json.Decode.succeed <| SetReverse False
                , onMouseLeave <| SetReverse False
                ]
                [ text "⏪" ]

        slowDown =
            button [ onClick SlowDown ] [ text "🐢" ]

        playingToggle =
            if playing then
                button [ onClick <| SetPlaying False ] [ text "⏸️" ]

            else
                button [ onClick <| SetPlaying True ] [ text "▶️" ]

        speedUp =
            button [ onClick SpeedUp ] [ text "🐇" ]
    in
    div
        [ classList
            [ ( "replay-controls", True )
            , ( "replay-controls--drag", model.drag /= Nothing )
            ]
        , style "top" (toFloat model.pos.y |> px)
        , style "left" (toFloat model.pos.x |> px)
        , on "mousedown" dragEventDecoder
        ]
        [ div [ class "buttons" ]
            [ rewind
            , slowDown
            , playingToggle
            , speedUp
            ]
        ]


webglView : Model -> Flags -> Assets.Model -> List WebGL.Entity
webglView { replay, started } flags assets =
    case replay of
        Just { state, usernamePa, usernamePb } ->
            let
                params =
                    GameState.paramsFromFlags flags
            in
            if started then
                GameState.webglView
                    (Started state)
                    { pa =
                        Just
                            { name = usernamePa
                            , xp = 0
                            , unlocks = Set.empty
                            , quests = []
                            }
                    , pb =
                        Just
                            { name = usernamePb
                            , xp = 0
                            , unlocks = Set.empty
                            , quests = []
                            }
                    }
                    Chat.init
                    params
                    assets
                    True

            else
                notStartedView params assets

        Nothing ->
            []


notStartedView : Render.Params -> Assets.Model -> List WebGL.Entity
notStartedView params assets =
    let
        ctx =
            bareContextInit ( params.w, params.h ) assets Mouse.NoMouse

        { radius, w, h } =
            ctx
    in
    List.concat
        [ Background.webglView params assets NullAnim
        , Font.view "Futura"
            "CLICK TO\nWATCH"
            { x = 0.5 * w
            , y = 0.5 * h - radius * 0.05
            , scaleX = 0.00045 * radius
            , scaleY = 0.00045 * radius
            , color = vec3 (244 / 255) (241 / 255) (94 / 255)
            }
            ctx
        ]
