module Listener exposing (..)

import Animation.Types exposing (Anim(..))
import Audio exposing (SoundOption(..), playSound, playSoundWith)
import GameState.State as GameState exposing (resolvable)
import GameState.Types exposing (GameState(..))
import Main.Messages exposing (Msg)
import Resolvable.State exposing (activeAnim, tickStart)
import Resolvable.Types as Resolvable


listen : Float -> GameState -> Cmd Msg
listen time state =
    let
        modelListen : Resolvable.Model -> Cmd Msg
        modelListen res =
            if tickStart res then
                let
                    sfxURL : Maybe String
                    sfxURL =
                        activeAnim res |> Maybe.andThen animSfx

                    animSfx : Anim -> Maybe String
                    animSfx anim =
                        case anim of
                            Slash _ _ ->
                                Just "slash.mp3"

                            Heal _ ->
                                Just "heal.mp3"

                            Draw _ ->
                                Just "draw.wav"

                            otherwise ->
                                Nothing

                    volume : Float
                    volume =
                        0.5
                in
                    case sfxURL of
                        Just url ->
                            playSoundWith
                                ("/sfx/" ++ url)
                                [ Volume volume ]

                        Nothing ->
                            Cmd.none
            else
                Cmd.none
    in
        case state of
            Selecting _ ->
                playSoundWith "/music/select.mp3" [ Loop, Once ]

            Started started ->
                modelListen <| resolvable started

            otherwise ->
                Cmd.none
