module Listener exposing (..)

import Audio exposing (SoundOption(..), playSound, playSoundWith)
import GameState exposing (GameState(..))
import Messages exposing (Msg)


listen : Float -> GameState -> Cmd Msg
listen time state =
    -- MAKE THIS NOT STUPID DUPLICATION
    case state of
        PlayingGame _ ( [], _ ) ->
            playSoundWith "music/background.mp3" [ Loop, Once ]

        PlayingGame m _ ->
            if GameState.tickZero state then
                let
                    sfxURL : String
                    sfxURL =
                        case List.head m.stack of
                            Nothing ->
                                ""

                            Just { card } ->
                                card.sfxURL

                    volume : Float
                    volume =
                        0.5 + 0.1 * (toFloat (List.length m.stack))
                in
                    playSoundWith ("sfx/" ++ sfxURL) [ Volume volume ]
            else
                Cmd.none

        Ended _ (Just m) _ ->
            if GameState.tickZero state then
                let
                    sfxURL : String
                    sfxURL =
                        case List.head m.stack of
                            Nothing ->
                                ""

                            Just { card } ->
                                card.sfxURL

                    volume : Float
                    volume =
                        0.5 + 0.1 * (toFloat (List.length m.stack))
                in
                    playSoundWith ("sfx/" ++ sfxURL) [ Volume volume ]
            else
                Cmd.none

        otherwise ->
            Cmd.none
