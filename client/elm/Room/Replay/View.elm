module Replay.View exposing (view)

import Animation.Types exposing (Anim(..))
import Background.View as Background
import Connected.View exposing (playersView)
import Endgame.View as Endgame
import GameState.Types exposing (GameState(..))
import GameState.View as GameState
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Replay.Types exposing (Model)
import Texture.Types as Texture


view : Model -> Flags -> Texture.Model -> Html Main.Msg
view { replay } flags textures =
    div [ class "replay" ] <|
        case replay of
            Just { state, usernamePa, usernamePb } ->
                [ playersView { pa = Just usernamePa, pb = Just usernamePb }
                , GameState.view (Started state) "" flags Nothing textures
                ]

            Nothing ->
                [ Background.view flags textures Finding
                , Endgame.view 0.0 NullAnim Nothing Nothing Nothing Nothing
                ]
