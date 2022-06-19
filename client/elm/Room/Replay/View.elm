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
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Math.Vector3 exposing (vec3)
import Mouse
import Render.Types as Render
import Replay.Types exposing (Model)
import Set
import WebGL


htmlView : Model -> Html Main.Msg
htmlView { replay, error } =
    case replay of
        Just { usernamePa, usernamePb } ->
            playersView
                { pa =
                    Just
                        { name = usernamePa
                        , xp = 0
                        , unlocks = Set.empty
                        }
                , pb =
                    Just
                        { name = usernamePb
                        , xp = 0
                        , unlocks = Set.empty
                        }
                }

        Nothing ->
            div [ class "error" ] [ text error ]


webglView : Model -> Flags -> Assets.Model -> List WebGL.Entity
webglView { replay, started } flags assets =
    case replay of
        Just { state } ->
            if started then
                GameState.webglView
                    (Started state)
                    Chat.init
                    (GameState.paramsFromFlags flags)
                    assets
                    True

            else
                notStartedView (GameState.paramsFromFlags flags) assets

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
