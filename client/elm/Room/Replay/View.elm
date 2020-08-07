module Replay.View exposing (htmlView, webglView)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Connected.View exposing (playersView)
import GameState.Types exposing (GameState(..))
import GameState.View as GameState
import Html exposing (Html, text)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Replay.Types exposing (Model)
import WebGL


htmlView : Model -> Html Main.Msg
htmlView { replay } =
    case replay of
        Just { usernamePa, usernamePb } ->
            playersView
                { pa = Just usernamePa, pb = Just usernamePb }

        Nothing ->
            text ""


webglView : Model -> Flags -> Assets.Model -> List WebGL.Entity
webglView { replay } flags assets =
    case replay of
        Just { state } ->
            GameState.webglView
                (Started state)
                (GameState.paramsFromFlags flags)
                assets

        Nothing ->
            []



-- [ Background.view flags assets Finding
-- , Endgame.view 0.0 NullAnim Nothing Nothing Nothing Nothing True flags.seed
-- ]
