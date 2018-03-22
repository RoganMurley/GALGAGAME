module Endgame.View exposing (view)

import Animation.State exposing (animToResTickMax)
import Animation.Types exposing (Anim(GameEnd))
import Connected.Messages as Connected
import Ease
import GameState.Messages exposing (Msg(GotoReplay, PlayingOnly), PlayingOnly(Rematch))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Main.Messages as Main
import Room.Messages as Room
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Float -> Maybe Anim -> Maybe String -> Html Main.Msg
view resTick anim mReplayId =
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
            progress < 0.8

        watchReplayButton =
            case mReplayId of
                Just replayId ->
                    button
                        [ class "replay"
                        , onClick <|
                            Main.RoomMsg <|
                                Room.ConnectedMsg <|
                                    Connected.GameStateMsg <|
                                        GotoReplay replayId
                        , disabled isDisabled
                        ]
                        [ text "Replay" ]

                Nothing ->
                    button
                        [ class "replay"
                        , disabled True
                        ]
                        [ text "Replay" ]
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
                , watchReplayButton
                ]
            ]
