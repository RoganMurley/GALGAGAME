module Endgame.View exposing (view)

import Animation.Types exposing (Anim(GameEnd))
import Connected.Messages as Connected
import GameState.Messages as GameState
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Events exposing (onClick)
import Main.Messages as Main
import PlayState.Messages exposing (Msg(GotoReplay, PlayingOnly), PlayingOnly(Rematch))
import Room.Messages as Room
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Float -> Anim -> Maybe String -> Maybe Int -> Html Main.Msg
view progress anim mReplayId mXp =
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
                                        GameState.PlayStateMsg <|
                                            GotoReplay replayId
                        , disabled isDisabled
                        ]
                        [ text "Replay" ]

                Nothing ->
                    button
                        [ class "replay", disabled True ]
                        [ text "Replay" ]

        experienceDisplay =
            case mXp of
                Just xp ->
                    div [ class "experience" ] [ text <| toString xp ++ "xp" ]

                Nothing ->
                    div [] []
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
            , div [ class "endgame-buttons" ]
                [ button
                    [ class "rematch"
                    , onClick <|
                        Main.RoomMsg <|
                            Room.ConnectedMsg <|
                                Connected.GameStateMsg <|
                                    GameState.PlayStateMsg <|
                                        PlayingOnly Rematch
                    , disabled isDisabled
                    ]
                    [ text "Rematch" ]
                , watchReplayButton
                ]
            , experienceDisplay
            ]
        ]
