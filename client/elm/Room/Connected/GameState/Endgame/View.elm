module Endgame.View exposing (view)

import Animation.State exposing (animToResTickMax)
import Animation.Types exposing (Anim(GameEnd))
import Connected.Messages as Connected
import Ease
import GameState.Messages exposing (Msg(PlayingOnly), PlayingOnly(Rematch, WatchReplay))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Main.Messages as Main
import Room.Messages as Room
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Float -> Maybe Anim -> Html Main.Msg
view resTick anim =
    let
        ( endGameText, endGameClass ) =
            case anim of
                Just (GameEnd (Just PlayerA)) ->
                    ( "VICTORY", "victory" )

                Just (GameEnd (Just PlayerB)) ->
                    ( "DEFEAT", "defeat" )

                Just (GameEnd Nothing) ->
                    ( "DRAW", "draw" )

                otherwise ->
                    ( "", "no-win" )

        resTickMax =
            animToResTickMax anim

        progress =
            Ease.outQuint <| resTick / resTickMax

        isDisabled =
            progress < 1.0
    in
        div
            [ classList
                [ ( "endgame-layer", True )
                , ( endGameClass, True )
                ]
            , style [ ( "opacity", toString progress ) ]
            ]
            [ div [ class "endgame-container" ]
                [ div
                    [ class endGameClass ]
                    [ text endGameText ]
                , button
                    [ class "rematch"
                    , onClick <|
                        Main.RoomMsg <|
                            Room.ConnectedMsg <|
                                Connected.GameStateMsg <|
                                    PlayingOnly Rematch
                    , disabled isDisabled
                    ]
                    [ text "Rematch" ]
                , button
                    [ class "replay"
                    , onClick <|
                        Main.RoomMsg <|
                            Room.ConnectedMsg <|
                                Connected.GameStateMsg <|
                                    PlayingOnly WatchReplay
                    , disabled isDisabled
                    ]
                    [ text "Replay" ]
                ]
            ]
