module Replay.View exposing (view)

import Connected.View exposing (playersView)
import Endgame.View as Endgame
import GameState.Types exposing (GameState(Started))
import GameState.View as GameState
import Html exposing (..)
import Html.Attributes exposing (..)
import Main.Types exposing (Flags)
import Main.Messages as Main
import Replay.Types exposing (..)
import Texture.Types as Texture


view : Model -> Flags -> Texture.Model -> Html Main.Msg
view { replay } flags textures =
    div [ class "replay" ] <|
        case replay of
            Just { state, usernamePa, usernamePb } ->
                [ playersView ( Just usernamePa, Just usernamePb )
                , GameState.view (Started state) "" flags textures
                ]

            Nothing ->
                [ div [ class "lds-facebook" ]
                    [ div [] []
                    , div [] []
                    , div [] []
                    ]
                , Endgame.view 0.0 Nothing Nothing
                ]
