module Listener exposing (..)

import Audio exposing (SoundOption(..), playSound, playSoundWith)
import GameState.State as GameState
import GameState.Types exposing (GameState(..))
import Model.Types exposing (Model)
import Main.Messages exposing (Msg)


listen : Float -> GameState -> Cmd Msg
listen time state =
    let
        modelListen : GameState -> Model -> Cmd Msg
        modelListen state m =
            if GameState.tickZero state then
                let
                    -- FIX ME, UNSAFE
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
                    playSoundWith ("/sfx/" ++ sfxURL) [ Volume volume ]
            else
                Cmd.none
    in
        case state of
            Selecting _ ->
                playSoundWith "/music/select.mp3" [ Loop, Once ]

            PlayingGame _ ( [], _ ) ->
                Cmd.none

            PlayingGame ( m, _ ) _ ->
                modelListen state m

            Ended _ (Just ( m, vm )) _ ->
                modelListen state m

            otherwise ->
                Cmd.none
