module Endgame.View exposing (view)

import Animation.Types exposing (Anim(GameEnd))
import Connected.Messages as Connected
import GameState.Messages as GameState
import GameType exposing (GameType(..))
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Events exposing (onClick)
import Main.Messages as Main
import PlayState.Messages exposing (Msg(GotoComputerGame, GotoReplay, PlayingOnly), PlayingOnly(Rematch))
import Room.Messages as Room
import Stats exposing (Experience, Level, StatChange)
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Float -> Anim -> Maybe String -> Maybe StatChange -> Maybe GameType -> Html Main.Msg
view progress anim mReplayId mXp gameType =
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

        rematchButton =
            case ( gameType, anim ) of
                ( Just TutorialGame, GameEnd (Just PlayerA) ) ->
                    button
                        [ class "rematch"
                        , onClick <|
                            Main.RoomMsg <|
                                Room.ConnectedMsg <|
                                    Connected.GameStateMsg <|
                                        GameState.PlayStateMsg <|
                                            GotoComputerGame
                        , disabled isDisabled
                        ]
                        [ text "Play Again" ]

                _ ->
                    button
                        [ class "rematch"
                        , onClick <|
                            Main.RoomMsg <|
                                Room.ConnectedMsg <|
                                    Connected.GameStateMsg <|
                                        GameState.PlayStateMsg <|
                                            PlayingOnly Rematch
                        , disabled isDisabled
                        ]
                        [ text "Play Again" ]

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
                        [ text "Watch Replay" ]

                Nothing ->
                    button
                        [ class "replay", disabled True ]
                        [ text "Watch Replay" ]

        experienceDisplay =
            case mXp of
                Just { initialExperience, finalExperience, initialLevel, finalLevel, nextLevelAt } ->
                    let
                        levelUp : Maybe Level
                        levelUp =
                            if initialLevel /= finalLevel then
                                Just finalLevel

                            else
                                Nothing

                        experienceChange : Experience
                        experienceChange =
                            finalExperience - initialExperience
                    in
                    div [ class "experience" ]
                        [ div []
                            [ text <| "+" ++ toString experienceChange ++ "xp" ]
                        , div [] <|
                            case levelUp of
                                Just _ ->
                                    [ text <| "LEVEL UP!" ]

                                Nothing ->
                                    []
                        , div []
                            [ text <| "Level " ++ toString finalLevel ++ " (" ++ toString finalExperience ++ "xp / " ++ toString nextLevelAt ++ "xp)"
                            ]
                        ]

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
                [ rematchButton
                , watchReplayButton
                ]
            , experienceDisplay
            ]
        ]
