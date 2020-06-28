module Replay.View exposing (view)

import Animation.Types exposing (Anim(..))
import Background.View as Background
import Connected.View exposing (playersView)
import Endgame.View as Endgame
import Font.Types as Font
import GameState.Types exposing (GameState(..))
import GameState.View as GameState
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Replay.Types exposing (Model)
import Texture.Types as Texture


view : Model -> Flags -> Texture.Model -> Font.Model -> Html Main.Msg
view { replay } flags textures fonts =
    div [ class "replay" ] <|
        case replay of
            Just { state, usernamePa, usernamePb } ->
                [ playersView { pa = Just usernamePa, pb = Just usernamePb }
                , GameState.view (Started state) "" flags Nothing True textures fonts
                ]

            Nothing ->
                [ Background.view flags textures fonts Finding
                , Endgame.view 0.0 NullAnim Nothing Nothing Nothing Nothing True flags.seed
                ]
