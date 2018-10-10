module Endgame.View exposing (view)

import Animation.Types exposing (Anim(GameEnd))
import Connected.Messages as Connected
import GameState.Messages exposing (Msg(GotoReplay, PlayingOnly), PlayingOnly(Rematch))
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Events exposing (onClick)
import Main.Messages as Main
import Room.Messages as Room
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Float -> Anim -> Maybe String -> Html Main.Msg
view progress anim mReplayId =
    let
        ( show, endGameText, endGameClass ) =
            case anim of
                GameEnd (Just PlayerA) ->
                    ( True, "VICTORY", "victory" )

                GameEnd (Just PlayerB) ->
                    ( True, "DEFEAT", "defeat" )

                GameEnd Nothing ->
                    ( True, "DRAW", "draw" )

                _ ->
                    ( False, "", "no-win" )

        isDisabled =
            not show && (progress < 0.8)

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
                        [ class "replay", disabled True ]
                        [ text "Replay" ]
    in
        div
            [ classList
                [ ( "endgame-layer", True )
                , ( endGameClass, True )
                ]
            , style
                [ if show then
                    ( "opacity", toString progress )
                  else
                    ( "", "" )
                ]
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
