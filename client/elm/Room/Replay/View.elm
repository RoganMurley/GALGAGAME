module Replay.View exposing (view)

import Connected.View exposing (playersView)
import Endgame.View as Endgame
import GameState.Types exposing (GameState(Started))
import GameState.View as GameState
import Html exposing (..)
import Html.Attributes exposing (..)
import Main.Types exposing (Flags)
import Main.Messages as Main
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch
import Replay.Types exposing (..)


view : Raymarch.Params -> Model -> Flags -> Html Main.Msg
view params { replay } ({ time } as flags) =
    let
        replayView : List (Html Main.Msg)
        replayView =
            case replay of
                Nothing ->
                    [ div [ class "lds-facebook" ]
                        [ div [] []
                        , div [] []
                        , div [] []
                        ]
                    , Endgame.view 0.0 Nothing Nothing
                    , Raymarch.view params
                    ]

                Just { state, usernamePa, usernamePb } ->
                    [ playersView ( Just usernamePa, Just usernamePb )
                    , GameState.view (Started state) "" flags
                    ]
    in
        div [ class "replay" ]
            replayView
