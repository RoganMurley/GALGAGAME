module Endgame.View exposing (view)

import Animation.Types exposing (Anim(..))
import Connected.Messages as Connected
import Endgame.Types exposing (Conversion(..))
import GameState.Messages as GameState
import GameType exposing (GameType(..))
import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (class, classList, disabled, href, style, target)
import Html.Events exposing (onClick)
import Main.Messages as Main
import Main.Types exposing (Seed)
import PlayState.Messages exposing (Msg(..), PlayingOnly(..))
import Random
import Room.Messages as Room
import Stats exposing (Experience, StatChange, levelAt, levelFromExperience, nextLevelAt)
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Float -> Anim -> Maybe String -> Maybe StatChange -> Maybe GameType -> Maybe String -> Seed -> Html Main.Msg
view progress anim mReplayId mXp gameType mUsername seed =
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

                ( Just DailyGame, GameEnd (Just PlayerA) ) ->
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
                        [ text "Play again" ]

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
                Just { initialXp, finalXp } ->
                    let
                        xp : Experience
                        xp =
                            initialXp + progress * (finalXp - initialXp)
                    in
                    div [ class "experience" ]
                        [ div [ class "experience-progress" ]
                            [ div
                                [ class "experience-level-badge" ]
                                [ text <| String.fromInt <| levelFromExperience xp ]
                            , div
                                [ class "experience-bar"
                                , style "width" <|
                                    String.fromFloat
                                        (100 * (xp - levelAt xp) / (nextLevelAt xp - levelAt xp))
                                        ++ "%"
                                ]
                                []
                            ]
                        ]

                Nothing ->
                    div [] []

        conversionLink =
            div [ class "endgame-conversion" ]
                [ case conversion of
                    Discord ->
                        a [ href "https://discord.gg/SVXXej4", target "_blank" ] [ text "Join the community on Discord" ]

                    Feedback ->
                        a [ href "/feedback", target "_blank" ] [ text "Submit feedback to the developers" ]

                    Signup ->
                        a [ href "/signup" ] [ text "Sign up to gain experience" ]
                ]

        styles =
            if show then
                style "opacity" <| String.fromFloat progress

            else
                style "" ""

        classes =
            classList
                [ ( "endgame-layer", True )
                , ( endGameClass, True )
                ]

        conversion =
            case mUsername of
                Just _ ->
                    Tuple.first <|
                        Random.step
                            (Random.uniform Discord [ Feedback ])
                            (Random.initialSeed seed)

                Nothing ->
                    Signup
    in
    div [ classes, styles ]
        [ div [ class "endgame-container" ]
            [ div
                [ class endGameClass ]
                [ text endGameText ]
            , experienceDisplay
            , div [ class "endgame-buttons" ]
                [ rematchButton
                , watchReplayButton
                ]
            , conversionLink
            ]
        ]
