module Replay.View exposing (view)

-- import Background.View as Background
-- import Endgame.View as Endgame

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Connected.View exposing (playersView)
import GameState.Types exposing (GameState(..))
import GameState.View as GameState
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Replay.Types exposing (Model)


view : Model -> Flags -> Assets.Model -> Html Main.Msg
view { replay } flags assets =
    div [ class "replay" ] <|
        case replay of
            Just { state, usernamePa, usernamePb } ->
                [ playersView { pa = Just usernamePa, pb = Just usernamePb }
                , GameState.view (Started state) "" flags assets
                ]

            Nothing ->
                []



-- [ Background.view flags assets Finding
-- , Endgame.view 0.0 NullAnim Nothing Nothing Nothing Nothing True flags.seed
-- ]
